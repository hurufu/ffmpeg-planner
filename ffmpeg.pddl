(define (domain ffmpeg)
    (:requirements
        :adl)

    (:types
        file stream codec string - object
        subtitles binary_stream - stream
        video_stream audio_stream - binary_stream
        audio_codec video_codec - codec)

    (:constants
        libx264 libx265 - video_codec
        aac opus - audio_codec)

    (:predicates
        (file_name ?f - file ?n - string)
        (encoded_by_video_codec ?s - stream ?c - video_codec)
        (encoded_by_audio_codec ?s - stream ?c - audio_codec))

    (:action input_file
        :parameters (?n - string)
        :vars (?f - file)
        :precondition (not (file_name ?f ?n))
        :effect (file_name ?f ?n))

    (:action use_video_codec
        :parameters (?s - stream ?c - video_codec)
        :precondition (not (exists (?c - video_codec) (encoded_by_video_codec ?s ?c)))
        :effect (encoded_by_video_codec ?s ?c))

    (:action use_audio_codec
        :parameters (?s - stream ?c - audio_codec)
        :precondition (not (exists (?c - audio_codec) (encoded_by_audio_codec ?s ?c)))
        :effect (encoded_by_audio_codec ?s ?c))

    (:action output_file
        :parameters (?n - string)
        :vars (?f - file)
        :precondition (not (file_name ?f ?n))
        :effect (file_name ?f ?n)))
