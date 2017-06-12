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
using Eto.Drawing;
using Eto.Forms;
using System.Timers;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public partial class GKPortrait : Panel
    {
        private PixelLayout fLayout;
        
        public Image Image
        {
            get { return pictureBox1.Image; }
            set { pictureBox1.Image = value; }
        }

        // FIXME: GKv3 DevRestriction
        /*public override Image BackgroundImage
        {
            get { return pictureBox1.BackgroundImage; }
            set { pictureBox1.BackgroundImage = value; }
        }*/

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

        // FIXME: GKv3 DevRestriction
        /*public PictureBoxSizeMode SizeMode
        {
            get { return pictureBox1.SizeMode; }
            set { pictureBox1.SizeMode = value;}
        }*/

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
            fLayout = new PixelLayout();
            InitializeComponent();
            fLayout.Move(btnPanel, 0, Height);
            timer.Stop();
        }

        /// <summary>
        /// Disposes resources used by the control.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
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
            Point p = fLayout.PointToClient(Mouse.Position);
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

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            btnPanel.Width = Width;
            CheckCursorPosition(this, e);
        }

        #region Design

        private ImageBox pictureBox1;
        private Panel btnPanel;
        private Timer timer;

        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
            pictureBox1 = new ImageBox();
            btnPanel = new Panel();
            timer = new Timer();
            SuspendLayout();

            //pictureBox1.BackgroundImageLayout = ImageLayout.Center;
            Content = pictureBox1;
            pictureBox1.Size = new Size(178, 188);
            pictureBox1.MouseLeave += PictureBox1MouseLeave;
            //pictureBox1.MouseHover += PictureBox1MouseHover;

            //btnPanel.BackgroundColor = SystemColors.ButtonShadow;
            //btnPanel.Location = new Point(0, 152);
            btnPanel.Size = new Size(178, 36);
            btnPanel.MouseLeave += Panel1MouseLeave;
            btnPanel.MouseMove += Panel1MouseHover; // MouseHover

            timer.Elapsed += MoveSlidePanel;

            fLayout.Add(btnPanel, 0, Height);

            Size = new Size(178, 188);
            ResumeLayout();
        }

        #endregion
    }
}
