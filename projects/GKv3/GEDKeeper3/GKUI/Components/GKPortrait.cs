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
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public class GKPortrait : Panel
    {
        private List<Button> fBtnsList = new List<Button>();
        private ImageBox fImageBox;
        private PixelLayout fLayout;
        private int fPixelSpeed = 5;
        private Panel fSlidePanel;
        private ITimer fTimer;


        public Image Image
        {
            get { return fImageBox.Image; }
            set { fImageBox.Image = value; }
        }

        public int SlidePanelHeight
        {
            get { return fSlidePanel.Height; }
            set { fSlidePanel.Height = value; }
        }

        public Panel SlidePanel
        {
            get { return fSlidePanel; }
        }

        public int PixelSpeed
        {
            get { return fPixelSpeed; }
            set { fPixelSpeed = value; }
        }


        public GKPortrait()
        {
            SuspendLayout();

            fLayout = new PixelLayout();

            fImageBox = new ImageBox();
            fImageBox.AllowZoom = false;
            fImageBox.SelectionMode = ImageBoxSelectionMode.None;
            fImageBox.Size = new Size(140, 140);
            fImageBox.MouseLeave += PictureBox1MouseLeave;
            //pictureBox1.MouseHover += PictureBox1MouseHover;

            fSlidePanel = new Panel();
            //fSlidePanel.BackgroundColor = SystemColors.ButtonShadow;
            //fSlidePanel.Location = new Point(0, 152);
            fSlidePanel.Size = new Size(178, 36);
            fSlidePanel.MouseLeave += Panel1MouseLeave;
            fSlidePanel.MouseMove += Panel1MouseHover; // MouseHover

            fLayout.Add(fImageBox, 0, 0);
            fLayout.Add(fSlidePanel, 0, Height);
            Content = fLayout;

            //Size = new Size(178, 188);
            ResumeLayout();

            fTimer = AppHost.Instance.CreateTimer(100.0f, MoveSlidePanel);

            fImageBox.Cursor = Cursors.Arrow;
            fSlidePanel.Cursor = Cursors.Arrow;

            fLayout.Move(fSlidePanel, 0, Height);
            fTimer.Stop();
        }

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
            /*int lenwagon = 0;

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
            }*/
        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            /*if (btnPanel.Top <= Height - btnPanel.Height)
                timer.Stop();
            else
                btnPanel.Top -= (btnPanel.Top - 5 > Height - btnPanel.Height) ? fPixelSpeed : btnPanel.Top - (Height - btnPanel.Height);*/
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
            /*Point p = fLayout.PointToClient(Mouse.Position);
            bool buf = (p.X <= 1 || p.Y <= 1 || p.X >= pictureBox1.Width || p.Y >= pictureBox1.Height - 1);
            if (!buf) {
                timer.Start();
                timer.Interval = 1;
            }
            else {
                btnPanel.Top = Height;
                timer.Stop();
            }*/
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            fSlidePanel.Width = Width;
            CheckCursorPosition(this, e);
        }
    }
}
