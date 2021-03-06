open Import
open Fiber.O

module Worker : sig
  (** Simple queue that is consumed by its own thread *)
  type 'work t

  val create : do_:('a -> unit) -> 'a t

  type task

  val cancel : task -> unit

  val add_work : 'a t -> 'a -> (task, [ `Stopped ]) result

  val stop : _ t -> unit
end = struct
  type state =
    | Running of Thread.t
    | Stopped of Thread.t
    | Finished

  type 'a t =
    { work : 'a Removable_queue.t
    ; mutable state : state
    ; mutex : Mutex.t
    ; work_available : Condition.t
    }

  and task = Task : 'a t * 'a Removable_queue.node -> task

  let cancel (Task (t, node)) =
    with_mutex t.mutex ~f:(fun () -> Removable_queue.remove node)

  let is_running t =
    match t.state with
    | Running _ -> true
    | Stopped _
    | Finished ->
      false

  let run (f, t) =
    let rec loop () =
      match t.state with
      | Stopped _ -> (
        match Removable_queue.pop t.work with
        | None -> t.state <- Finished
        | Some job -> do_work job)
      | Finished -> ()
      | Running _ -> (
        match Removable_queue.pop t.work with
        | Some job -> do_work job
        | None ->
          while Removable_queue.is_empty t.work && is_running t do
            Condition.wait t.work_available t.mutex
          done;
          loop ())
    and do_work job =
      Mutex.unlock t.mutex;
      f job;
      Mutex.lock t.mutex;
      loop ()
    in
    with_mutex t.mutex ~f:loop

  let create ~do_ =
    let t =
      { work = Removable_queue.create ()
      ; state = Finished
      ; mutex = Mutex.create ()
      ; work_available = Condition.create ()
      }
    in
    t.state <- Running (Thread.create run (do_, t));
    t

  let add_work (type a) (t : a t) (w : a) =
    with_mutex t.mutex ~f:(fun () ->
        if is_running t then (
          let node = Removable_queue.push t.work w in
          Condition.signal t.work_available;
          Ok (Task (t, node))
        ) else
          Error `Stopped)

  let stop (t : _ t) =
    with_mutex t.mutex ~f:(fun () ->
        match t.state with
        | Running th ->
          t.state <- Stopped th;
          Condition.signal t.work_available
        | Stopped _
        | Finished ->
          ())
end

module Timer_id = Id.Make ()

type process =
  { pid : Pid.t
  ; ivar : Unix.process_status Fiber.Ivar.t
  }

type process_state =
  | Running of process
  | Zombie of Unix.process_status

type t =
  { mutable events_pending : int
  ; events : event Queue.t
  ; events_mutex : Mutex.t
  ; time_mutex : Mutex.t
  ; event_ready : Condition.t
  ; timers_available : Condition.t
  ; timers_available_mutex : Mutex.t
  ; timer_resolution : float
  ; mutable threads : thread list
  ; mutable time : Thread.t
  ; (* TODO Replace with Removable_queue *)
    timers : (Timer_id.t, active_timer ref) Table.t
  ; process_watcher : process_watcher Lazy.t
  }

and event =
  | Job_completed : 'a * 'a Fiber.Ivar.t -> event
  | Scheduled of active_timer
  | Abort

and job =
  | Pending :
      (unit -> 'a)
      * ('a, [ `Exn of Exn_with_backtrace.t | `Canceled ]) result Fiber.Ivar.t
      -> job

and thread =
  { scheduler : t
  ; worker : job Worker.t
  }

and timer =
  { mutable delay : float
  ; timer_scheduler : t
  ; timer_id : Timer_id.t
  }

and active_timer =
  { scheduled : float
  ; ivar : [ `Resolved | `Cancelled ] Fiber.Ivar.t
  ; parent : timer
  }

and process_watcher =
  { mutex : Mutex.t
  ; something_is_running : Condition.t
  ; table : (Pid.t, process_state) Table.t
  ; process_scheduler : t
  ; mutable running_count : int
  }

let add_events t = function
  | [] -> ()
  | events ->
    with_mutex t.events_mutex ~f:(fun () ->
        List.iter events ~f:(Queue.push t.events);
        Condition.signal t.event_ready)

let is_empty table = Table.length table = 0

let me = Fiber.Var.create ()

let signal_timers_available t =
  with_mutex t.timers_available_mutex ~f:(fun () ->
      Condition.signal t.timers_available)

let time_loop t =
  let rec loop () =
    let to_run = ref [] in
    with_mutex t.time_mutex ~f:(fun () ->
        if not (is_empty t.timers) then
          let now = Unix.gettimeofday () in
          Table.filteri_inplace t.timers ~f:(fun ~key:_ ~data:active_timer ->
              let active_timer = !active_timer in
              let scheduled_at =
                active_timer.scheduled +. active_timer.parent.delay
              in
              let need_to_run = scheduled_at < now in
              if need_to_run then to_run := active_timer :: !to_run;
              not need_to_run));
    let to_run =
      List.sort !to_run ~compare:(fun x y ->
          Timer_id.compare x.parent.timer_id y.parent.timer_id)
      |> List.map ~f:(fun x -> Scheduled x)
    in
    add_events t to_run;
    Unix.sleepf t.timer_resolution;
    loop ()
  in
  loop ()

let create_thread () =
  let+ scheduler = Fiber.Var.get_exn me in
  let worker =
    let do_ (Pending (f, ivar)) =
      let res =
        match Exn_with_backtrace.try_with f with
        | Ok x -> Ok x
        | Error exn -> Error (`Exn exn)
      in
      add_events scheduler [ Job_completed (res, ivar) ]
    in
    Worker.create ~do_
  in
  let t = { scheduler; worker } in
  scheduler.threads <- t :: scheduler.threads;
  t

let add_pending_events t by =
  with_mutex t.events_mutex ~f:(fun () ->
      t.events_pending <- t.events_pending + by;
      assert (t.events_pending >= 0))

type 'a task =
  { ivar :
      ('a, [ `Exn of Exn_with_backtrace.t | `Canceled ]) result Fiber.Ivar.t
  ; task : Worker.task
  }

let await task = Fiber.Ivar.read task.ivar

let await_no_cancel task =
  let open Fiber.O in
  let+ res = Fiber.Ivar.read task.ivar in
  match res with
  | Ok x -> Ok x
  | Error `Canceled -> assert false
  | Error (`Exn exn) -> Error exn

let cancel_task task =
  let open Fiber.O in
  let* status = Fiber.Ivar.peek task.ivar in
  match status with
  | Some _ -> Fiber.return ()
  | None ->
    Worker.cancel task.task;
    Fiber.Ivar.fill task.ivar (Error `Canceled)

let async (t : thread) f =
  add_pending_events t.scheduler 1;
  let ivar = Fiber.Ivar.create () in
  let work = Worker.add_work t.worker (Pending (f, ivar)) in
  Result.map work ~f:(fun task -> { ivar; task })

let async_exn t f =
  match async t f with
  | Error `Stopped -> Code_error.raise "async_exn: stopped thread" []
  | Ok task -> task

let stop (t : thread) = Worker.stop t.worker

let cancel_timers () =
  let* t = Fiber.Var.get_exn me in
  let timers = ref [] in
  with_mutex t.time_mutex ~f:(fun () ->
      Table.filteri_inplace t.timers ~f:(fun ~key:_ ~data:timer ->
          timers := !timer.ivar :: !timers;
          false));
  Fiber.parallel_iter !timers ~f:(fun ivar -> Fiber.Ivar.fill ivar `Cancelled)

type run_error =
  | Never
  | Abort_requested
  | Exn of Exn_with_backtrace.t

exception Abort of run_error

let () =
  Printexc.register_printer (function
    | Abort Never -> Some "Abort: Never"
    | Abort Abort_requested -> Some "Abort: requested"
    | Abort (Exn exn) ->
      Some
        ("Abort: " ^ Format.asprintf "%a@." Exn_with_backtrace.pp_uncaught exn)
    | _ -> None)

let event_next (t : t) : Fiber.fill =
  with_mutex t.events_mutex ~f:(fun () ->
      while Queue.is_empty t.events do
        Condition.wait t.event_ready t.events_mutex
      done;
      let consume_event () =
        let res = Queue.pop_exn t.events in
        t.events_pending <- t.events_pending - 1;
        assert (t.events_pending >= 0);
        res
      in
      if Queue.is_empty t.events then
        Error (Abort Never)
      else
        match consume_event () with
        | Abort -> Error (Abort Abort_requested)
        | Job_completed (a, ivar) -> Ok (Fiber.Fill (ivar, a))
        | Scheduled active_timer -> Ok (Fill (active_timer.ivar, `Resolved)))
  |> Result.ok_exn

let report t =
  let status m =
    if Mutex.try_lock m then
      "was unlocked"
    else
      "locked"
  in
  [ ("time_mutex", t.time_mutex)
  ; ("timers_available_mutex", t.timers_available_mutex)
  ; ("events_mutex", t.events_mutex)
  ]
  |> List.iter ~f:(fun (name, mutex) ->
         Format.eprintf "%s: %s@." name (status mutex));
  Format.eprintf "pending events: %d@." t.events_pending;
  Format.eprintf "events: %d@." (Queue.length t.events);
  Format.eprintf "threads: %d@." (List.length t.threads);
  Format.eprintf "timers: %d@." (Table.length t.timers)

let iter (t : t) =
  if t.events_pending = 0 then (
    let () = assert (Queue.is_empty t.events) in
    report t;
    raise (Abort Never)
  ) else
    event_next t

let create_timer ~delay =
  let+ t = Fiber.Var.get_exn me in
  { timer_scheduler = t; delay; timer_id = Timer_id.gen () }

let set_delay t ~delay = t.delay <- delay

let schedule (type a) (timer : timer) (f : unit -> a Fiber.t) :
    (a, [ `Cancelled ]) result Fiber.t =
  let open Fiber.O in
  let active_timer =
    let scheduled = Unix.gettimeofday () in
    { scheduled; ivar = Fiber.Ivar.create (); parent = timer }
  in
  let* () =
    match
      with_mutex timer.timer_scheduler.time_mutex ~f:(fun () ->
          match Table.find timer.timer_scheduler.timers timer.timer_id with
          | Some active ->
            let to_cancel = !active.ivar in
            active := active_timer;
            `Cancel to_cancel
          | None ->
            Table.add_exn timer.timer_scheduler.timers timer.timer_id
              (ref active_timer);
            `Signal_timers_available)
    with
    | `Cancel ivar -> Fiber.Ivar.fill ivar `Cancelled
    | `Signal_timers_available ->
      add_pending_events timer.timer_scheduler 1;
      signal_timers_available timer.timer_scheduler;
      Fiber.return ()
  in
  let* res = Fiber.Ivar.read active_timer.ivar in
  match res with
  | `Cancelled as e -> Fiber.return (Error e)
  | `Resolved ->
    let+ res = f () in
    Ok res

let cancel_timer (timer : timer) =
  let t = timer.timer_scheduler in
  match
    with_mutex t.time_mutex ~f:(fun () ->
        match Table.find t.timers timer.timer_id with
        | None -> None
        | Some at ->
          Table.remove t.timers timer.timer_id;
          Some !at.ivar)
  with
  | None -> Fiber.return ()
  | Some ivar ->
    with_mutex t.events_mutex ~f:(fun () ->
        t.events_pending <- t.events_pending - 1);
    Fiber.Ivar.fill ivar `Cancelled

let abort () =
  (* TODO proper cleanup *)
  let+ t = Fiber.Var.get_exn me in
  add_events t [ Abort ]

module Process_watcher : sig
  val init : t -> process_watcher

  (** Register a new running process. *)
  val register : process_watcher -> process -> unit

  (** Send the following signal to all running processes. *)
  val killall : process_watcher -> int -> unit
end = struct
  module Process_table : sig
    val add : process_watcher -> process -> unit

    val remove : process_watcher -> pid:Pid.t -> Unix.process_status -> unit

    val running_count : process_watcher -> int

    val iter : process_watcher -> f:(process -> unit) -> unit
  end = struct
    let add t job =
      match Table.find t.table job.pid with
      | None ->
        Table.set t.table job.pid (Running job);
        t.running_count <- t.running_count + 1;
        if t.running_count = 1 then Condition.signal t.something_is_running
      | Some (Zombie status) ->
        Table.remove t.table job.pid;
        add_events t.process_scheduler [ Job_completed (status, job.ivar) ]
      | Some (Running _) -> assert false

    let remove t ~pid status =
      match Table.find t.table pid with
      | None -> Table.set t.table pid (Zombie status)
      | Some (Running job) ->
        t.running_count <- t.running_count - 1;
        Table.remove t.table pid;
        add_events t.process_scheduler [ Job_completed (status, job.ivar) ]
      | Some (Zombie _) -> assert false

    let iter t ~f =
      Table.iter t.table ~f:(fun data ->
          match data with
          | Running job -> f job
          | Zombie _ -> ())

    let running_count t = t.running_count
  end

  let register t process =
    add_pending_events t.process_scheduler 1;
    Mutex.lock t.mutex;
    Process_table.add t process;
    Mutex.unlock t.mutex

  let killall t signal =
    Mutex.lock t.mutex;
    Process_table.iter t ~f:(fun job ->
        try Unix.kill (Pid.to_int job.pid) signal with
        | Unix.Unix_error _ -> ());
    Mutex.unlock t.mutex

  exception Finished of process * Unix.process_status

  let wait_nonblocking_win32 t =
    try
      Process_table.iter t ~f:(fun job ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
          if pid <> 0 then raise_notrace (Finished (job, status)));
      false
    with
    | Finished (job, status) ->
      (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
      Process_table.remove t ~pid:job.pid status;
      true

  let wait_win32 t =
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock t.mutex;
      Thread.delay 0.001;
      Mutex.lock t.mutex
    done

  let wait_unix t =
    Mutex.unlock t.mutex;
    let pid, status = Unix.wait () in
    Mutex.lock t.mutex;
    let pid = Pid.of_int pid in
    Process_table.remove t ~pid status

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

  let run t =
    Mutex.lock t.mutex;
    while true do
      while Process_table.running_count t = 0 do
        Condition.wait t.something_is_running t.mutex
      done;
      wait t
    done

  let init process_scheduler =
    let t =
      { mutex = Mutex.create ()
      ; something_is_running = Condition.create ()
      ; table = Table.create (module Pid) 128
      ; running_count = 0
      ; process_scheduler
      }
    in
    ignore (Thread.create run t : Thread.t);
    t
end

let cleanup t =
  List.iter t.threads ~f:stop;
  if Lazy.is_val t.process_watcher then
    Process_watcher.killall (Lazy.force t.process_watcher) Sys.sigkill

let create () =
  let rec t =
    { events_pending = 0
    ; events = Queue.create ()
    ; events_mutex = Mutex.create ()
    ; time_mutex = Mutex.create ()
    ; event_ready = Condition.create ()
    ; timer_resolution = 0.1
    ; threads = []
    ; timers = Table.create (module Timer_id) 10
    ; timers_available = Condition.create ()
    ; timers_available_mutex = Mutex.create ()
    ; time = Thread.self ()
    ; process_watcher
    }
  and process_watcher = lazy (Process_watcher.init t) in
  t.time <- Thread.create time_loop t;
  t

let run_result : 'a. 'a Fiber.t -> ('a, _) result =
 fun f ->
  let t = create () in
  let f = Fiber.Var.set me t (fun () -> f) in
  let iter () = iter t in
  let res =
    match Fiber.run f ~iter with
    | exception Abort err -> Error err
    | exception exn ->
      let exn = Exn_with_backtrace.capture exn in
      Error (Exn exn)
    | res ->
      assert (t.events_pending = 0);
      Ok res
  in
  cleanup t;
  res

let run f =
  match run_result f with
  | Ok s -> s
  | Error e -> raise (Abort e)

let wait_for_process pid =
  let* t = Fiber.Var.get_exn me in
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register (Lazy.force t.process_watcher) { pid; ivar };
  Fiber.Ivar.read ivar
