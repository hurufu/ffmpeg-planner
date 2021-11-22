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

            (opened ?f)
            (written ?f)
            (pending ?s))

        (:action OPEN_FILE
            :parameters (?in ?if)
            :precondition (and (input-file ?if)
                               (name ?in)
                               (have ?if ?in))
            :effect (opened ?if))

        (:action MAP_AUDIO_STREAM
            :parameters (?if ?is ?os)
            :precondition (and (opened ?if)
                               (audio-stream ?is)
                               (have ?if ?is)
                               (not (= ?is ?os))
                               (not (have ?if ?os)))
            :effect (and (audio-stream ?os)
                         (stream ?os)
                         (pending ?os)))

        (:action MAP_VIDEO_STREAM
            :parameters (?if ?is ?os)
            :precondition (and (opened ?if)
                               (video-stream ?is)
                               (have ?if ?is)
                               (not (= ?is ?os))
                               (not (have ?if ?os)))
            :effect (and (video-stream ?os)
                         (stream ?os)
                         (pending ?os)))

        (:action SET_AUDIO_ENCODER
            :parameters (?os ?oc)
            :precondition (and (audio-codec ?oc)
                               (audio-stream ?os)
                               (pending ?os))
            :effect (encode ?oc ?os))

        (:action SET_VIDEO_ENCODER
            :parameters (?os ?oc)
            :precondition (and (video-codec ?oc)
                               (video-stream ?os)
                               (pending ?os))
            :effect (encode ?oc ?os))

        (:action WRITE_FILE
            :parameters (?on ?of)
            :effect (and (output-file ?of)
                         (name ?on)
                         (file ?of)
                         (have ?of ?on)
                         (forall (?s)
                                 (when (pending ?s)
                                       (and (have ?of ?s)
                                            (not (pending ?s))))))))
