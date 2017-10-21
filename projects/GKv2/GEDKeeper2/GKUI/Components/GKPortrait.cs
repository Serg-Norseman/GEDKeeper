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
    public class GKPortrait : UserControl
    {
        private int fPixelSpeed = 5;
        private readonly List<Button> fBtnsList = new List<Button>();


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
                lenwagon += (i > 0) ? (8 + fBtnsList[i].Width) : fBtnsList[i].Width;
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
        private PictureBox pictureBox1;
        private Panel btnPanel;
        private Timer timer;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            components = new System.ComponentModel.Container();
            pictureBox1 = new PictureBox();
            btnPanel = new Panel();
            timer = new Timer(components);
            ((System.ComponentModel.ISupportInitialize)(pictureBox1)).BeginInit();
            SuspendLayout();

            pictureBox1.BackgroundImageLayout = ImageLayout.Center;
            pictureBox1.Dock = DockStyle.Fill;
            pictureBox1.Location = new Point(0, 0);
            pictureBox1.Name = "pictureBox1";
            pictureBox1.Size = new Size(178, 188);
            pictureBox1.TabIndex = 0;
            pictureBox1.TabStop = false;
            pictureBox1.MouseLeave += PictureBox1MouseLeave;
            pictureBox1.MouseHover += PictureBox1MouseHover;

            btnPanel.BackColor = SystemColors.ButtonShadow;
            btnPanel.Location = new Point(0, 152);
            btnPanel.Name = "panel1";
            btnPanel.Size = new Size(178, 36);
            btnPanel.TabIndex = 1;
            btnPanel.MouseLeave += Panel1MouseLeave;
            btnPanel.MouseHover += Panel1MouseHover;

            timer.Tick += MoveSlidePanel;

            AutoScaleDimensions = new SizeF(6F, 13F);
            AutoScaleMode = AutoScaleMode.Font;
            Controls.Add(btnPanel);
            Controls.Add(pictureBox1);
            Name = "GKPortrait";
            Size = new Size(178, 188);
            ((System.ComponentModel.ISupportInitialize)(pictureBox1)).EndInit();
            ResumeLayout(false);
        }

        #endregion
    }
}
