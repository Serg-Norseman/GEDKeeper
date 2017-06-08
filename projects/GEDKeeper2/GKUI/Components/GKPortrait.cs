/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih, Igor Tyulyakov.
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
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public partial class GKPortrait : UserControl
    {
        public Image Image
        {
            get { return pictureBox1.Image; }
            set { pictureBox1.Image = value; }
        }

        public override Image BackgroundImage
        {
            get { return pictureBox1.BackgroundImage; }
            set { pictureBox1.BackgroundImage = value; }
        }

        public int SlidePanelHeight
        {
            get { return btnPanel.Height; }
            set { btnPanel.Height = value; }
        }

        public Panel SlidePanel
        {
            get { return btnPanel; }
            set { btnPanel = value; }
        }

        public int PixelSpeed
        {
            get { return fPixelSpeed; }
            set { fPixelSpeed = value; }
        }

        public PictureBoxSizeMode SizeMode
        {
            get { return pictureBox1.SizeMode; }
            set { pictureBox1.SizeMode = value;}
        }

        public override Cursor Cursor
        {
            get {
                return base.Cursor;
            }
            set {
                base.Cursor = value;
                pictureBox1.Cursor = value;
                btnPanel.Cursor = value;
            }
        }

        private int fPixelSpeed = 5;
        private List<Button> fBtnsList = new List<Button>();

        public GKPortrait()
        {
            InitializeComponent();
            btnPanel.Top = Height;
            timer.Stop();
        }

        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            RedrawButtons();
        }

        private void RedrawButtons()
        {
            int lenwagon = 0;

            btnPanel.Controls.Clear();

            for (int i = 0, c = fBtnsList.Count; i < c; i++)
            {
                lenwagon += (i != c) ? (fBtnsList[i].Width + 8) : fBtnsList[i].Width;
            }

            int center = lenwagon / 2;
            int startPosition = btnPanel.Width / 2 - center;

            for (int i = 0, c = fBtnsList.Count; i < c; i++)
            {
                int heightCenter = btnPanel.Height / 2;
                int btnCenter = fBtnsList[i].Height / 2;

                fBtnsList[i].Location = new Point(startPosition, heightCenter - btnCenter);
                btnPanel.Controls.Add(fBtnsList[i]);
                startPosition += fBtnsList[i].Width + 8;
            }
        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            if (btnPanel.Top <= Height - btnPanel.Height)
                timer.Stop();
            else
                btnPanel.Top -= (btnPanel.Top - 5 > Height - btnPanel.Height) ? fPixelSpeed : btnPanel.Top - (Height - btnPanel.Height);
        }

        private void PictureBox1MouseHover(object sender, EventArgs e)
        {
            CheckCursorPosition(sender, e);
        }

        private void Panel1MouseHover(object sender, EventArgs e)
        {
            CheckCursorPosition(sender, e);
        }

        private void PictureBox1MouseLeave(object sender, EventArgs e)
        {
            CheckCursorPosition(sender, e);
        }

        private void Panel1MouseLeave(object sender, EventArgs e)
        {
            CheckCursorPosition(sender, e);
        }

        private void CheckCursorPosition(object sender, EventArgs e)
        {
            Point p = PointToClient(Cursor.Position);
            bool buf = (p.X <= 1 || p.Y <= 1 || p.X >= pictureBox1.Width || p.Y >= pictureBox1.Height - 1);
            if (!buf) {
                timer.Start();
                timer.Interval = 1;
            }
            else {
                btnPanel.Top = Height;
                timer.Stop();
            }
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            btnPanel.Width = Width;
            CheckCursorPosition(this, e);
        }

        #region Design

        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Panel btnPanel;
        private System.Windows.Forms.Timer timer;

        /// <summary>
        /// Disposes resources used by the control.
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
            components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GKPortrait));
            pictureBox1 = new System.Windows.Forms.PictureBox();
            btnPanel = new System.Windows.Forms.Panel();
            timer = new System.Windows.Forms.Timer(components);
            ((System.ComponentModel.ISupportInitialize)(pictureBox1)).BeginInit();
            SuspendLayout();

            pictureBox1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center;
            pictureBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            pictureBox1.Location = new System.Drawing.Point(0, 0);
            pictureBox1.Name = "pictureBox1";
            pictureBox1.Size = new System.Drawing.Size(178, 188);
            pictureBox1.TabIndex = 0;
            pictureBox1.TabStop = false;
            pictureBox1.MouseLeave += new System.EventHandler(PictureBox1MouseLeave);
            pictureBox1.MouseHover += new System.EventHandler(PictureBox1MouseHover);

            btnPanel.BackColor = System.Drawing.SystemColors.ButtonShadow;
            btnPanel.Location = new System.Drawing.Point(0, 152);
            btnPanel.Name = "panel1";
            btnPanel.Size = new System.Drawing.Size(178, 36);
            btnPanel.TabIndex = 1;
            btnPanel.MouseLeave += new System.EventHandler(Panel1MouseLeave);
            btnPanel.MouseHover += new System.EventHandler(Panel1MouseHover);

            timer.Tick += new System.EventHandler(MoveSlidePanel);

            AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            Controls.Add(btnPanel);
            Controls.Add(pictureBox1);
            Name = "GKPortrait";
            Size = new System.Drawing.Size(178, 188);
            ((System.ComponentModel.ISupportInitialize)(pictureBox1)).EndInit();
            ResumeLayout(false);
        }

        #endregion
    }
}
