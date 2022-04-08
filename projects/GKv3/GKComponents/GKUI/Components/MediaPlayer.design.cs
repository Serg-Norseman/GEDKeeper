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
            btnPause.Image = UIHelper.LoadResourceImage("Resources.btn_pause.png");
            btnPause.ImagePosition = ButtonImagePosition.Overlay;
            btnPause.Size = new Size(40, 40);
            btnPause.Click += btnPause_Click;

            btnPlay = new Button();
            btnPlay.Image = UIHelper.LoadResourceImage("Resources.btn_play.png");
            btnPlay.ImagePosition = ButtonImagePosition.Overlay;
            btnPlay.Size = new Size(40, 40);
            btnPlay.Click += btnPlay_Click;

            btnStop = new Button();
            btnStop.Image = UIHelper.LoadResourceImage("Resources.btn_stop.png");
            btnStop.ImagePosition = ButtonImagePosition.Overlay;
            btnStop.Size = new Size(40, 40);
            btnStop.Click += btnStop_Click;

            //

            btnMute = new Button();
            btnMute.Image = UIHelper.LoadResourceImage("Resources.btn_volume_mute.png");
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
                        Cells = { TableLayout.Horizontal(trkPosition, lblDuration) }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(btnPause, btnPlay, btnStop, null, btnMute, trkVolume) }
                    }
                }
            };

            pnlVideo = new Panel();
            pnlVideo.BackgroundColor = SystemColors.ControlText;

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
