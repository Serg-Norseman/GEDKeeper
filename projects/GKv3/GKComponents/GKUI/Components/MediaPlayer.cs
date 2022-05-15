/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2022 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Runtime.InteropServices;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Interfaces;
using LibVLCSharp.Shared;
using VLCMediaPlayer = LibVLCSharp.Shared.MediaPlayer;

namespace GKUI.Components
{
    public partial class MediaPlayer : Panel, ILocalizable
    {
        #region Design components

        private Panel pnlVideo;
        private Button btnMute;
        private Slider trkVolume;
        private Button btnPause;
        private Button btnPlay;
        private Button btnStop;
        private Slider trkPosition;
        private Label lblDuration;

        #endregion

        private LibVLC fLibVLC;
        private Media fMedia;
        private string fMediaFile;
        private VLCMediaPlayer fPlayer;

        public string MediaFile
        {
            get { return fMediaFile; }
            set { fMediaFile = value; }
        }

        public MediaPlayer()
        {
            XamlReader.Load(this);

            btnPause.Image = UIHelper.LoadResourceImage("Resources.btn_pause.png");
            btnPlay.Image = UIHelper.LoadResourceImage("Resources.btn_play.png");
            btnStop.Image = UIHelper.LoadResourceImage("Resources.btn_stop.png");
            btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_mute.png");

            Core.Initialize();
            fLibVLC = new LibVLC();
            fMedia = null;
            fPlayer = null;

            trkVolume.Value = Math.Max(0, 100);
            trkVolume_Scroll(null, null);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fMedia != null) {
                    fMedia.Dispose();
                }
                if (fPlayer != null) {
                    fPlayer.Stop();
                    MpAttach(IntPtr.Zero);
                    fPlayer.Dispose();
                }
                fLibVLC.Dispose();
            }
            base.Dispose(disposing);
        }

        private void Play()
        {
            if (fMedia == null) {
                if (string.IsNullOrEmpty(fMediaFile)) {
                    AppHost.StdDialogs.ShowError("Please select media path first");
                    return;
                }

                fMedia = new Media(fLibVLC, fMediaFile);
                fMedia.DurationChanged += Events_DurationChanged;
                fMedia.StateChanged += Events_StateChanged;
                fMedia.ParsedChanged += Events_ParsedChanged;

                //System.Diagnostics.Debug.Print(this.ParentWindow.NativeHandle.ToString());
                //System.Diagnostics.Debug.Print(this.NativeHandle.ToString());

                MpAttach(IntPtr.Zero);
                fPlayer = new VLCMediaPlayer(fMedia);
                MpAttach(pnlVideo.NativeHandle);

                fPlayer.PositionChanged += Events_PlayerPositionChanged;
                fPlayer.TimeChanged += Events_TimeChanged;
                fPlayer.EndReached += Events_MediaEnded;
                fPlayer.Stopped += Events_PlayerStopped;

                fMedia.Parse();
            }

            fPlayer.Play();
        }

        private void MpAttach(IntPtr handle)
        {
            if (fPlayer == null || fPlayer.NativeReference == IntPtr.Zero)
                return;

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                fPlayer.Hwnd = handle;
            } else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) {
                fPlayer.NsObject = handle;
            } else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) {
                fPlayer.XWindow = (uint)handle;
            } else {
                throw new InvalidOperationException("Unsupported OSPlatform");
            }
        }

        public void SetLocale()
        {
        }

        private void InitControls()
        {
            trkPosition.Value = 0;
            lblDuration.Text = @"00:00:00 / 00:00:00";
        }

        #region Event handlers

        private void Events_PlayerStopped(object sender, EventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                InitControls();
            });
        }

        private void Events_MediaEnded(object sender, EventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                InitControls();
            });
        }

        private void Events_TimeChanged(object sender, MediaPlayerTimeChangedEventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                //lblTime.Text = TimeSpan.FromMilliseconds(e.NewTime).ToString().Substring(0, 8);
            });
        }

        private void Events_PlayerPositionChanged(object sender, MediaPlayerPositionChangedEventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                int newPos = (int)(e.Position * 100);
                if (newPos > trkPosition.MaxValue) return;
                trkPosition.Value = newPos;
            });
        }

        private void Events_StateChanged(object sender, MediaStateChangedEventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                //label1.Text = e.State.ToString();
            });
        }

        private void Events_DurationChanged(object sender, MediaDurationChangedEventArgs e)
        {
            Application.Instance.Invoke(delegate () {
                lblDuration.Text = TimeSpan.FromMilliseconds(e.Duration).ToString().Substring(0, 8);
            });
        }

        private void Events_ParsedChanged(object sender, MediaParsedChangedEventArgs e)
        {
            Console.WriteLine(e.ParsedStatus);
        }

        #endregion

        #region Controls handlers

        private void btnPlay_Click(object sender, EventArgs e)
        {
            Play();
        }

        private void trkVolume_Scroll(object sender, EventArgs e)
        {
            if (fPlayer == null)
                return;

            fPlayer.Mute = false;
            fPlayer.Volume = trkVolume.Value;

            if (fPlayer.Volume <= 100 && fPlayer.Volume > 50) {
                btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_max.png");
            }
            if (fPlayer.Volume <= 50 && fPlayer.Volume > 5) {
                btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_middle.png");
            }
            if (fPlayer.Volume <= 5 && fPlayer.Volume > 0) {
                btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_min.png");
            }
            if (fPlayer.Volume == 0) {
                btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_mute.png");
            }
        }

        private void trkPosition_Scroll(object sender, EventArgs e)
        {
            if (fPlayer == null)
                return;

            fPlayer.Position = trkPosition.Value / 100.0f;
        }

        public void btnStop_Click(object sender, EventArgs e)
        {
            if (fPlayer == null)
                return;

            fPlayer.Stop();
        }

        private void btnPause_Click(object sender, EventArgs e)
        {
            if (fPlayer == null)
                return;

            fPlayer.Pause();
        }

        private void btnMute_Click(object sender, EventArgs e)
        {
            if (fPlayer == null)
                return;

            fPlayer.ToggleMute();

            if (fPlayer.Mute) {
                btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_mute.png");
            } else {
                trkVolume_Scroll(sender, e);
            }
        }

        #endregion
    }
}
