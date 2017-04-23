/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Drawing.Imaging;
using System.Windows.Forms;

using GKCommon;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Charts
{
    public abstract class CustomChart : ScrollablePanel, IPrintable
    {
        private static readonly object EventNavRefresh;


        private readonly NavigationStack fNavman;


        public event EventHandler NavRefresh
        {
            add { Events.AddHandler(EventNavRefresh, value); }
            remove { Events.RemoveHandler(EventNavRefresh, value); }
        }


        static CustomChart()
        {
            EventNavRefresh = new object();
        }

        protected CustomChart() : base()
        {
            fNavman = new NavigationStack();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fNavman != null) fNavman.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override bool IsInputKey(Keys keyData)
        {
            switch (keyData) {
                case Keys.Left:
                case Keys.Right:
                case Keys.Up:
                case Keys.Down:
                case Keys.Back:
                    return true;

                default:
                    return base.IsInputKey(keyData);
            }
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            e.Handled = true;
            switch (e.KeyCode) {
                case Keys.Left:
                    HorizontalScroll.Value =
                        Math.Max(HorizontalScroll.Value - HorizontalScroll.SmallChange, 0);
                    PerformLayout();
                    break;

                case Keys.Right:
                    HorizontalScroll.Value += HorizontalScroll.SmallChange;
                    PerformLayout();
                    break;

                case Keys.Up:
                    VerticalScroll.Value =
                        Math.Max(VerticalScroll.Value - VerticalScroll.SmallChange, 0);
                    PerformLayout();
                    break;

                case Keys.Down:
                    VerticalScroll.Value += VerticalScroll.SmallChange;
                    PerformLayout();
                    break;

                case Keys.PageUp:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value =
                            Math.Max(VerticalScroll.Value - VerticalScroll.LargeChange, 0);
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value =
                            Math.Max(HorizontalScroll.Value - HorizontalScroll.LargeChange, 0);
                    }
                    PerformLayout();
                    break;

                case Keys.PageDown:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value += VerticalScroll.LargeChange;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value += HorizontalScroll.LargeChange;
                    }
                    PerformLayout();
                    break;

                case Keys.Home:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value = 0;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value = 0;
                    }
                    PerformLayout();
                    break;

                case Keys.End:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value = VerticalScroll.Maximum;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value = HorizontalScroll.Maximum;
                    }
                    PerformLayout();
                    break;

                case Keys.Back:
                    NavPrev();
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (MouseButtons.XButton1 == e.Button) {
                NavPrev();
            } else if (MouseButtons.XButton2 == e.Button) {
                NavNext();
            } else {
                base.OnMouseUp(e);
            }
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (Keys.None == ModifierKeys) {
                VerticalScroll.Value = Math.Max(VerticalScroll.Value - e.Delta, 0);
                PerformLayout();
            } else if (Keys.Shift == ModifierKeys) {
                HorizontalScroll.Value = Math.Max(HorizontalScroll.Value - e.Delta, 0);
                PerformLayout();
            }
            else {
                base.OnMouseWheel(e);
            }
        }

        #region Print and snaphots support

        public abstract Size GetImageSize();
        public abstract void RenderStaticImage(Graphics gfx, bool printer);

        public bool IsLandscape()
        {
            Size imageSize = GetImageSize();
            return (imageSize.Height < imageSize.Width);
        }

        public Image GetPrintableImage()
        {
            Size imageSize = GetImageSize();
            var frameRect = new Rectangle(0, 0, imageSize.Width, imageSize.Height);

            Image image;
            using (var gfx = CreateGraphics()) {
                image = new Metafile(gfx.GetHdc(), frameRect, MetafileFrameUnit.Pixel, EmfType.EmfOnly);
            }

            using (Graphics gfx = Graphics.FromImage(image)) {
                RenderStaticImage(gfx, true);
            }

            return image;
        }

        /* TODO(zsv): Need to find an appropriate icon in the general style
         * for the main toolbar - screenshot capture for windows with charts. */
        public void SaveSnapshot(string fileName)
        {
            string ext = SysUtils.GetFileExtension(fileName);

            Size imageSize = GetImageSize();
            if ((ext == ".bmp" || ext == ".jpg") && imageSize.Width >= 65535)
            {
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
            }
            else
            {
                ImageFormat imFmt = ImageFormat.Png;
                if (ext == ".bmp") { imFmt = ImageFormat.Bmp; }
                else
                    if (ext == ".emf") { imFmt = ImageFormat.Emf; }
                else
                    if (ext == ".png") { imFmt = ImageFormat.Png; }
                else
                    if (ext == ".gif") { imFmt = ImageFormat.Gif; }
                else
                    if (ext == ".jpg") { imFmt = ImageFormat.Jpeg; }

                Image pic;
                if (Equals(imFmt, ImageFormat.Emf)) {
                    using (var gfx = CreateGraphics()) {
                        pic = new Metafile(fileName, gfx.GetHdc());
                    }
                } else {
                    pic = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
                }

                try
                {
                    using (Graphics gfx = Graphics.FromImage(pic)) {
                        RenderStaticImage(gfx, false);
                    }

                    pic.Save(fileName, imFmt);
                }
                finally
                {
                    pic.Dispose();
                }
            }
        }

        #endregion

        #region Navigation support

        private void DoNavRefresh()
        {
            var eventHandler = (EventHandler)Events[EventNavRefresh];
            if (eventHandler == null) return;

            eventHandler(this, null);
        }

        protected abstract void SetNavObject(object obj);

        public bool NavAdd(object obj)
        {
            if (obj != null && !fNavman.Busy) {
                fNavman.Current = obj;
                return true;
            }
            return false;
        }

        public bool NavCanBackward()
        {
            return fNavman.CanBackward();
        }

        public bool NavCanForward()
        {
            return fNavman.CanForward();
        }

        public void NavNext()
        {
            if (!fNavman.CanForward()) return;

            fNavman.BeginNav();
            try
            {
                SetNavObject(fNavman.Next());
                DoNavRefresh();
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        public void NavPrev()
        {
            if (!fNavman.CanBackward()) return;

            fNavman.BeginNav();
            try
            {
                SetNavObject(fNavman.Back());
                DoNavRefresh();
            }
            finally
            {
                fNavman.EndNav();
            }
        }

        #endregion
    }
}
