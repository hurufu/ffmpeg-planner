(define (problem test01)
    (:domain ffmpeg)
    (:objects
        f0 f1 - file
        s00 s10 - video_stream
        s01 s11 - audio_stream
        hello world - string)
    (:init
        (file_name f0 world)
        (has_stream f0 s00)
        (has_stream f0 s01)
        (encoded_by_video_codec s00 libx264)
        (encoded_by_audio_codec s01 aac))
    (:goal (and
        (encoded_by_video_codec s10 libx265)
        (encoded_by_audio_codec s11 opus)
        (has_stream f1 s10)
        (has_stream f1 s11)
        (file_name f1 hello))))
