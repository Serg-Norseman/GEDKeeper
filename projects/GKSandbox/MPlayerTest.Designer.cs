/*
 * Created by SharpDevelop.
 * User: Norseman
 * Date: 20.02.2017
 * Time: 22:39
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
namespace GKSandbox
{
    partial class MPlayerTest
    {
        /// <summary>
        /// Designer variable used to keep track of non-visual components.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        
        /// <summary>
        /// Disposes resources used by the form.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
            this.txtFileName = new System.Windows.Forms.TextBox();
            this.btnOpen = new System.Windows.Forms.Button();
            this.mediaPlayer1 = new GKMediaPlayer.MediaPlayer();
            this.SuspendLayout();
            // 
            // txtFileName
            // 
            this.txtFileName.Location = new System.Drawing.Point(68, 12);
            this.txtFileName.Name = "txtFileName";
            this.txtFileName.Size = new System.Drawing.Size(433, 20);
            this.txtFileName.TabIndex = 13;
            // 
            // btnOpen
            // 
            this.btnOpen.Location = new System.Drawing.Point(9, 9);
            this.btnOpen.Name = "btnOpen";
            this.btnOpen.Size = new System.Drawing.Size(53, 23);
            this.btnOpen.TabIndex = 12;
            this.btnOpen.Text = "Open";
            this.btnOpen.UseVisualStyleBackColor = true;
            this.btnOpen.Click += new System.EventHandler(this.btnOpen_Click);
            // 
            // mediaPlayer1
            // 
            this.mediaPlayer1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.mediaPlayer1.Location = new System.Drawing.Point(0, 38);
            this.mediaPlayer1.MediaFile = null;
            this.mediaPlayer1.Name = "mediaPlayer1";
            this.mediaPlayer1.Size = new System.Drawing.Size(802, 372);
            this.mediaPlayer1.TabIndex = 14;
            // 
            // MPlayerTest
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(802, 410);
            this.Controls.Add(this.mediaPlayer1);
            this.Controls.Add(this.txtFileName);
            this.Controls.Add(this.btnOpen);
            this.Name = "MPlayerTest";
            this.Text = "MPlayerTest";
            this.ResumeLayout(false);
            this.PerformLayout();
        }
        private GKMediaPlayer.MediaPlayer mediaPlayer1;
        private System.Windows.Forms.Button btnOpen;
        private System.Windows.Forms.TextBox txtFileName;
    }
}
