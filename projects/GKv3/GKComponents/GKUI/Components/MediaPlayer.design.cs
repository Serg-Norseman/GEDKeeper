using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    partial class MediaPlayer
    {
        private Panel pnlVideo;
        private Panel pnlControls;
        private Button btnMute;
        private Slider trkVolume;
        private Button btnPause;
        private Button btnPlay;
        private Button btnStop;
        private Slider trkPosition;
        private Label lblDuration;

        private void InitializeComponent()
        {
            SuspendLayout();

            trkPosition = new Slider();
            trkPosition.MaxValue = 100;
            trkPosition.Size = new Size(610, 32);
            trkPosition.ValueChanged += trkPosition_Scroll;

            lblDuration = new Label();
            lblDuration.Size = new Size(49, 13);
            lblDuration.Text = "00:00:00 / 00:00:00";

            //

            btnPause = new Button();
            btnPause.Image = Bitmap.FromResource("Resources.btnPause");
            btnPause.ImagePosition = ButtonImagePosition.Overlay;
            btnPause.Size = new Size(40, 40);
            btnPause.Click += btnPause_Click;

            btnPlay = new Button();
            btnPlay.Image = Bitmap.FromResource("Resources.btnPlay");
            btnPlay.ImagePosition = ButtonImagePosition.Overlay;
            btnPlay.Size = new Size(40, 40);
            btnPlay.Click += btnPlay_Click;

            btnStop = new Button();
            btnStop.Image = Bitmap.FromResource("Resources.btnStop");
            btnStop.ImagePosition = ButtonImagePosition.Overlay;
            btnStop.Size = new Size(40, 40);
            btnStop.Click += btnStop_Click;

            //

            btnMute = new Button();
            btnMute.Image = Bitmap.FromResource("Resources.btnVolumeMute.gif");
            btnMute.ImagePosition = ButtonImagePosition.Overlay;
            btnMute.Size = new Size(40, 40);
            btnMute.Click += btnMute_Click;

            trkVolume = new Slider();
            trkVolume.MaxValue = 100;
            trkVolume.Size = new Size(116, 45);
            //trkVolume.TickStyle = TickStyle.None;
            trkVolume.ValueChanged += trkVolume_Scroll;

            pnlControls = new Panel();
            pnlControls.Content = new TableLayout() {
                Rows = {
                    new TableRow {
                        Cells = { trkPosition, lblDuration }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(btnPause, btnPlay, btnStop, null), TableLayout.Horizontal(btnMute, trkVolume) }
                    }
                }
            };

            pnlVideo = new Panel();
            pnlVideo.BackgroundColor = SystemColors.ControlText;
            //pnlVideo.BackgroundImage = global::ExtResources.pnlVideo;
            //pnlVideo.BackgroundImageLayout = ImageLayout.Center;
            //pnlVideo.BorderStyle = BorderStyle.Fixed3D;

            Content = new TableLayout() {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { pnlVideo }
                    },
                    new TableRow {
                        Cells = { pnlControls }
                    }
                }
            };

            //Size = new Size(770, 457);

            ResumeLayout();
        }
    }
}
