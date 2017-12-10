# The firefork_audio_player application #

This application allows playing single mp3 files or their playlists - m3u.

It depends on OS software - mplayer, so this package has to be installed to
the device before using this application. On Debian based systems install by:
```
apt-get install mplayer
```

## Play modes
There are two play modes: single and playlist. The mode is automatically
decided by the file extension: mp3, m3u.

In order to create m3u file from all the mp3 files in the directory,
run the following:
```
ls *.mp3 > playlist_name.m3u
```

## Usage
Play single mp3 file:
```
ok = firefork_audio_player:play("/path/to/file.mp3").
```
Example:
```
ok = firefork_audio_player:play("~/Downloads/Santi.mp3").
```
Play playlist m3u file:
```
ok = firefork_audio_player:play("~/Downloads/playlist.m3u").
```
Stop:
```
firefork_audio_player:stop().
```
Pause:
```
firefork_audio_player:pause().
```
Resume:
```
firefork_audio_player:resume().
```
Next song (available only when playing a playlist):
```
firefork_audio_player:next().
```
Previous song (available only when playing a playlist):
```
firefork_audio_player:prev().
```
Running the app as standalone
```
ERL_FLAGS="-config test/sys.config" rebar3 shell --apps firefork_audio_player
```
