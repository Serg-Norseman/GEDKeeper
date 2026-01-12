/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih, Igor Tyulyakov.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using GKCore.Design.Controls;

namespace GKUI.Components
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public class GKPortrait : UserControl, IPortraitControl
    {
        private IContainer components = null;
        private PictureBox fImageBox;
        private Panel fSlidePanel;
        private Timer fTimer;

        private readonly List<Button> fBtnsList;
        private int fPixelSpeed;


        [Browsable(false)]
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        public Image Image
        {
            get { return fImageBox.Image; }
            set {
                if (fImageBox.Image != null) {
                    fImageBox.Image.Dispose();
                }
                fImageBox.Image = value;
            }
        }


        public GKPortrait()
        {
            //BorderStyle = BorderStyle.FixedSingle;

            components = new Container();
            fTimer = new Timer(components);
            fTimer.Tick += MoveSlidePanel;
            fTimer.Stop();

            fImageBox = new PictureBox();
            fImageBox.BackgroundImageLayout = ImageLayout.Center;
            fImageBox.Dock = DockStyle.Fill;
            fImageBox.MouseLeave += CheckCursorPosition;
            fImageBox.MouseHover += CheckCursorPosition;
            fImageBox.SizeMode = PictureBoxSizeMode.Zoom;
            fImageBox.Cursor = Cursors.Arrow;

            fSlidePanel = new Panel();
            fSlidePanel.BackColor = SystemColors.ButtonShadow;
            fSlidePanel.Location = new Point(0, 152);
            fSlidePanel.Size = new Size(178, 36);
            fSlidePanel.MouseLeave += CheckCursorPosition;
            fSlidePanel.MouseHover += CheckCursorPosition;
            fSlidePanel.Height = 36;
            fSlidePanel.Cursor = Cursors.Arrow;
            fSlidePanel.Top = Height;

            SuspendLayout();
            Controls.Add(fSlidePanel);
            Controls.Add(fImageBox);
            Name = "GKPortrait";
            ResumeLayout(false);

            fBtnsList = new List<Button>();
            fPixelSpeed = 5;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Select();
        }

        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            RedrawButtons();
        }

        private void RedrawButtons()
        {
            int lenwagon = 0;

            fSlidePanel.Controls.Clear();

            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                lenwagon += (i > 0) ? (8 + fBtnsList[i].Width) : fBtnsList[i].Width;
            }

            int center = lenwagon / 2;
            int startPosition = fSlidePanel.Width / 2 - center;

            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                int heightCenter = fSlidePanel.Height / 2;
                int btnCenter = fBtnsList[i].Height / 2;

                fBtnsList[i].Location = new Point(startPosition, heightCenter - btnCenter);
                fSlidePanel.Controls.Add(fBtnsList[i]);
                startPosition += fBtnsList[i].Width + 8;
            }
        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            if (fSlidePanel.Top <= Height - fSlidePanel.Height) {
                fTimer.Stop();
            } else {
                fSlidePanel.Top -= (fSlidePanel.Top - 5 > Height - fSlidePanel.Height) ? fPixelSpeed : fSlidePanel.Top - (Height - fSlidePanel.Height);
            }
        }

        private void CheckCursorPosition(object sender, EventArgs e)
        {
            Point p = PointToClient(Cursor.Position);
            bool buf = (p.X <= 1 || p.Y <= 1 || p.X >= fImageBox.Width || p.Y >= fImageBox.Height - 1);
            if (!buf) {
                fTimer.Start();
                fTimer.Interval = 1;
            } else {
                fSlidePanel.Top = Height;
                fTimer.Stop();
            }
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);

            fSlidePanel.Width = Width;
            fSlidePanel.Top = Height;
            CheckCursorPosition(this, e);
        }
    }
}
