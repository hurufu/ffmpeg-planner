(define (domain ffmpeg)
        (:requirements :equality)
        (:constants Libopus Aac Libx264 Libx265)
        (:predicates
            (file ?f)
            (input-file ?f)
            (output-file ?f)
            (name ?n)
            (stream ?s)
            (video-stream ?s)
            (audio-stream ?s)
            (codec ?c)
            (video-codec ?c)
            (audio-codec ?c)

            (have ?a ?b)
            (encode ?c ?s)

            (opened-file ?f)
            (written-file ?f)
            (written ?f)
            (used-name ?n)
            (pending-stream ?s))

        (:action OPEN_FILE
            :parameters (?in ?if)
            :precondition (and (input-file ?if)
                               (name ?in)
                               (have ?if ?in))
            :effect (and (opened-file ?if) (used-name ?in)))

        (:action MARK_STREAM
            :parameters (?s)
            :precondition (not (or (file ?s) (name ?s) (stream ?s) (codec ?s)))
            :effect (stream ?s))
        (:action MARK_AUDIO_STREAM
            :parameters (?s)
            :precondition (and (stream ?s)
                               (not (or (audio-stream ?s) (video-stream ?s))))
            :effect (audio-stream ?s))
        (:action MARK_VIDEO_STREAM
            :parameters (?s)
            :precondition (and (stream ?s)
                               (not (or (audio-stream ?s) (video-stream ?s))))
            :effect (video-stream ?s))
        (:action MARK_NAME
            :parameters (?n)
            :precondition (not (or (file ?n) (name ?n) (stream ?n) (codec ?n)))
            :effect (name ?n))
        (:action MARK_FILE
            :parameters (?f)
            :precondition (not (or (file ?f) (name ?f) (stream ?f) (codec ?f)))
            :effect (file ?f))
        (:action MARK_OUTPUT_FILE
            :parameters (?of)
            :precondition (and (file ?of) (not (or (output-file ?of) (input-file ?of))))
            :effect (output-file ?of))

        (:action MAP_AUDIO_STREAM
            :parameters (?if ?is ?os)
            :precondition (and (opened-file ?if) (audio-stream ?is) (audio-stream ?os)
                               (have ?if ?is)
                               (not (or (have ?if ?os) (pending-stream ?os))))
            :effect (pending-stream ?os))
        (:action MAP_VIDEO_STREAM
            :parameters (?if ?is ?os)
            :precondition (and (opened-file ?if) (video-stream ?is) (video-stream ?os)
                               (have ?if ?is)
                               (not (or (have ?if ?os) (pending-stream ?os))))
            :effect (pending-stream ?os))

        (:action SET_AUDIO_ENCODER
            :parameters (?os ?oc)
            :precondition (and (audio-codec ?oc) (audio-stream ?os) (pending-stream ?os))
            :effect (encode ?oc ?os))
        (:action SET_VIDEO_ENCODER
            :parameters (?os ?oc)
            :precondition (and (video-codec ?oc) (video-stream ?os) (pending-stream ?os))
            :effect (encode ?oc ?os))

        (:action WRITE_FILE
            :parameters (?on ?of)
            :precondition (and (name ?on) (output-file ?of)
                               (exists (?if) (opened-file ?if))
                               (not (used-name ?on))
                               (not (written-file ?of)))
            :effect (and (have ?of ?on)
                         (written-file ?of)
                         (forall (?s)
                                 (when (pending-stream ?s)
                                       (and (have ?of ?s)
                                            (not (pending-stream ?s))))))))
