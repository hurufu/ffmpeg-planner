(define (problem test01)
    (:domain ffmpeg)
    (:objects
        f0 f1 - file
        s00 s10 - video_stream
        s01 s11 - audio_stream
        hello world - string)
    (:init
        (file_name f0 world)
        (encoded_by_video_codec s00 libx264)
        (encoded_by_audio_codec s01 aac))
    (:goal (and
        (encoded_by_video_codec s10 libx265)
        (encoded_by_audio_codec s11 opus)
        (file_name f1 hello))))
