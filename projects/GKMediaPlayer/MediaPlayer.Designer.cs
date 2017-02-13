namespace GKMediaPlayer
{
    partial class MediaPlayer
    {
        private System.Windows.Forms.Panel pnlControls;
        private System.Windows.Forms.Panel pnlVideo;
        private System.Windows.Forms.Button btnMute;
        private System.Windows.Forms.Button btnPause;
        private System.Windows.Forms.Button btnPlay;
        private System.Windows.Forms.Button btnStop;
        private System.Windows.Forms.TrackBar trkPosition;
        private System.Windows.Forms.TrackBar trkVolume;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label lblDuration;
        private System.Windows.Forms.Label lblTime;
        private System.Windows.Forms.Label label2;
        private System.ComponentModel.IContainer components = null;

        private void InitializeComponent()
        {
            this.pnlVideo = new System.Windows.Forms.Panel();
            this.btnMute = new System.Windows.Forms.Button();
            this.btnPause = new System.Windows.Forms.Button();
            this.btnPlay = new System.Windows.Forms.Button();
            this.btnStop = new System.Windows.Forms.Button();
            this.trkPosition = new System.Windows.Forms.TrackBar();
            this.trkVolume = new System.Windows.Forms.TrackBar();
            this.label1 = new System.Windows.Forms.Label();
            this.lblTime = new System.Windows.Forms.Label();
            this.lblDuration = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.pnlControls = new System.Windows.Forms.Panel();
            ((System.ComponentModel.ISupportInitialize)(this.trkPosition)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.trkVolume)).BeginInit();
            this.pnlControls.SuspendLayout();
            this.SuspendLayout();
            // 
            // pnlVideo
            // 
            this.pnlVideo.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.pnlVideo.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlVideo.Location = new System.Drawing.Point(0, 0);
            this.pnlVideo.Margin = new System.Windows.Forms.Padding(4);
            this.pnlVideo.Name = "pnlVideo";
            this.pnlVideo.Size = new System.Drawing.Size(1026, 562);
            this.pnlVideo.TabIndex = 0;
            // 
            // btnMute
            // 
            this.btnMute.Location = new System.Drawing.Point(151, 72);
            this.btnMute.Margin = new System.Windows.Forms.Padding(4);
            this.btnMute.Name = "btnMute";
            this.btnMute.Size = new System.Drawing.Size(53, 28);
            this.btnMute.TabIndex = 1;
            this.btnMute.Text = "Mute";
            this.btnMute.UseVisualStyleBackColor = true;
            this.btnMute.Click += new System.EventHandler(this.btnMute_Click);
            // 
            // btnPause
            // 
            this.btnPause.Location = new System.Drawing.Point(359, 72);
            this.btnPause.Margin = new System.Windows.Forms.Padding(4);
            this.btnPause.Name = "btnPause";
            this.btnPause.Size = new System.Drawing.Size(65, 28);
            this.btnPause.TabIndex = 2;
            this.btnPause.Text = "Pause";
            this.btnPause.UseVisualStyleBackColor = true;
            this.btnPause.Click += new System.EventHandler(this.btnPause_Click);
            // 
            // btnPlay
            // 
            this.btnPlay.Location = new System.Drawing.Point(432, 72);
            this.btnPlay.Margin = new System.Windows.Forms.Padding(4);
            this.btnPlay.Name = "btnPlay";
            this.btnPlay.Size = new System.Drawing.Size(65, 28);
            this.btnPlay.TabIndex = 3;
            this.btnPlay.Text = "Play";
            this.btnPlay.UseVisualStyleBackColor = true;
            this.btnPlay.Click += new System.EventHandler(this.btnPlay_Click);
            // 
            // btnStop
            // 
            this.btnStop.Location = new System.Drawing.Point(505, 72);
            this.btnStop.Margin = new System.Windows.Forms.Padding(4);
            this.btnStop.Name = "btnStop";
            this.btnStop.Size = new System.Drawing.Size(65, 28);
            this.btnStop.TabIndex = 4;
            this.btnStop.Text = "Stop";
            this.btnStop.UseVisualStyleBackColor = true;
            this.btnStop.Click += new System.EventHandler(this.btnStop_Click);
            // 
            // trkPosition
            // 
            this.trkPosition.Dock = System.Windows.Forms.DockStyle.Top;
            this.trkPosition.Location = new System.Drawing.Point(8, 8);
            this.trkPosition.Margin = new System.Windows.Forms.Padding(4);
            this.trkPosition.Maximum = 100;
            this.trkPosition.Name = "trkPosition";
            this.trkPosition.Size = new System.Drawing.Size(1010, 56);
            this.trkPosition.TabIndex = 6;
            this.trkPosition.Scroll += new System.EventHandler(this.trkPosition_Scroll);
            // 
            // trkVolume
            // 
            this.trkVolume.Location = new System.Drawing.Point(212, 72);
            this.trkVolume.Margin = new System.Windows.Forms.Padding(4);
            this.trkVolume.Maximum = 100;
            this.trkVolume.Name = "trkVolume";
            this.trkVolume.Size = new System.Drawing.Size(139, 56);
            this.trkVolume.TabIndex = 7;
            this.trkVolume.Scroll += new System.EventHandler(this.trkVolume_Scroll);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 695);
            this.label1.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(0, 17);
            this.label1.TabIndex = 8;
            // 
            // lblTime
            // 
            this.lblTime.AutoSize = true;
            this.lblTime.Location = new System.Drawing.Point(647, 78);
            this.lblTime.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblTime.Name = "lblTime";
            this.lblTime.Size = new System.Drawing.Size(64, 17);
            this.lblTime.TabIndex = 10;
            this.lblTime.Text = "00:00:00";
            // 
            // lblDuration
            // 
            this.lblDuration.AutoSize = true;
            this.lblDuration.Location = new System.Drawing.Point(745, 78);
            this.lblDuration.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblDuration.Name = "lblDuration";
            this.lblDuration.Size = new System.Drawing.Size(64, 17);
            this.lblDuration.TabIndex = 11;
            this.lblDuration.Text = "00:00:00";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(721, 78);
            this.label2.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(12, 17);
            this.label2.TabIndex = 12;
            this.label2.Text = "/";
            // 
            // pnlControls
            // 
            this.pnlControls.Controls.Add(this.label2);
            this.pnlControls.Controls.Add(this.trkPosition);
            this.pnlControls.Controls.Add(this.lblDuration);
            this.pnlControls.Controls.Add(this.btnMute);
            this.pnlControls.Controls.Add(this.lblTime);
            this.pnlControls.Controls.Add(this.btnPause);
            this.pnlControls.Controls.Add(this.btnPlay);
            this.pnlControls.Controls.Add(this.btnStop);
            this.pnlControls.Controls.Add(this.trkVolume);
            this.pnlControls.Controls.Add(this.label1);
            this.pnlControls.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.pnlControls.Location = new System.Drawing.Point(0, 422);
            this.pnlControls.Name = "pnlControls";
            this.pnlControls.Padding = new System.Windows.Forms.Padding(8);
            this.pnlControls.Size = new System.Drawing.Size(1026, 140);
            this.pnlControls.TabIndex = 13;
            // 
            // MediaPlayer
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.pnlControls);
            this.Controls.Add(this.pnlVideo);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "MediaPlayer";
            this.Size = new System.Drawing.Size(1026, 562);
            ((System.ComponentModel.ISupportInitialize)(this.trkPosition)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.trkVolume)).EndInit();
            this.pnlControls.ResumeLayout(false);
            this.pnlControls.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
