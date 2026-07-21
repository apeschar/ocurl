(** Asynchronous API with Lwt *)

(** A pool backed by a single libcurl multi handle. *)
module Pool : sig
  type t

  (** Create an independent pool.

      The pool must eventually be released with {!cleanup}. *)
  val create : unit -> t

  (** Set an option on the pool's multi handle.

      Raises [Invalid_argument] if the pool has been cleaned up. *)
  val setopt : t -> Curl.Multi.curlMultiOption -> unit

  (** Perform a transfer asynchronously using this pool.

      A handle must not be used by more than one active transfer at a time.
      Cancelling the returned promise removes the handle from the pool.

      Raises [Invalid_argument] if the pool has been cleaned up or the handle is
      already active in this pool.

      @return the transfer result code *)
  val perform : t -> Curl.t -> Curl.curlCode Lwt.t

  (** Cancel active transfers and destroy the pool's multi handle.

      Active transfer promises are cancelled. Easy handles are removed from the
      pool but are not passed to {!Curl.cleanup}, and may be reused. Calling
      [cleanup] more than once has no effect. *)
  val cleanup : t -> unit

  (** [with_pool f] creates a pool, calls [f pool], and cleans up the pool when
      the returned promise terminates. *)
  val with_pool : (t -> 'a Lwt.t) -> 'a Lwt.t
end

(** Perform a transfer asynchronously using the process-global default pool.

    @return the transfer result code *)
val perform : Curl.t -> Curl.curlCode Lwt.t

(** Set an option on the process-global default pool. *)
val setopt : Curl.Multi.curlMultiOption -> unit
