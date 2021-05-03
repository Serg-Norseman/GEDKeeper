/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GDModel;
using GKCore;
using GKCore.Charts;

namespace GKUI.Components
{
    public abstract class CustomChart : ScrollablePanel, IPrintable
    {
        private static readonly object EventNavRefresh;


        private readonly NavigationStack<GDMRecord> fNavman;
        protected ChartRenderer fRenderer;


        public event EventHandler NavRefresh
        {
            add { Events.AddHandler(EventNavRefresh, value); }
            remove { Events.RemoveHandler(EventNavRefresh, value); }
        }


        static CustomChart()
        {
            EventNavRefresh = new object();
        }

        protected CustomChart()
        {
            fNavman = new NavigationStack<GDMRecord>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //if (fNavman != null) fNavman.Dispose();
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
            } else {
                base.OnMouseWheel(e);
            }
        }

        #region Print and snaphots support

        protected Rectangle GetImageViewPort()
        {
            Rectangle viewPort;

            var imageSize = GetImageSize();
            if (!imageSize.IsEmpty) {
                Rectangle clientRect = GetClientRect(true);

                int x = !HScroll ? (clientRect.Width - (imageSize.Width + Padding.Horizontal)) / 2 : 0;
                int y = !VScroll ? (clientRect.Height - (imageSize.Height + Padding.Vertical)) / 2 : 0;

                int width = Math.Min(imageSize.Width - Math.Abs(AutoScrollPosition.X), clientRect.Width);
                int height = Math.Min(imageSize.Height - Math.Abs(AutoScrollPosition.Y), clientRect.Height);

                viewPort = new Rectangle(x + clientRect.Left, y + clientRect.Top, width, height);
            } else {
                viewPort = Rectangle.Empty;
            }

            return viewPort;
        }

        public abstract ExtSize GetImageSize();
        public abstract void RenderImage(RenderTarget target, bool forciblyCentered = false);

        public bool IsLandscape()
        {
            ExtSize imageSize = GetImageSize();
            return (imageSize.Height < imageSize.Width);
        }

        public IImage GetPrintableImage()
        {
            ExtSize imageSize = GetImageSize();
            var frameRect = new Rectangle(0, 0, imageSize.Width, imageSize.Height);

            Image image;
            using (var gfx = CreateGraphics()) {
                image = new Metafile(gfx.GetHdc(), frameRect, MetafileFrameUnit.Pixel, EmfType.EmfOnly);
            }

            using (Graphics gfx = Graphics.FromImage(image)) {
                fRenderer.SetTarget(gfx);
                RenderImage(RenderTarget.Printer);
            }

            return new ImageHandler(image);
        }

        /* TODO(zsv): Need to find an appropriate icon in the general style
         * for the main toolbar - screenshot capture for windows with charts. */
        public void SaveSnapshot(string fileName)
        {
            string ext = FileHelper.GetFileExtension(fileName);

            ExtSize imageSize = GetImageSize();

            if (ext == ".svg") {
                var prevRenderer = fRenderer;
                SetRenderer(new SVGRenderer(fileName, imageSize.Width, imageSize.Height));
                fRenderer.BeginDrawing();
                try {
                    using (var gfx = CreateGraphics()) {
                        fRenderer.SetTarget(gfx);

                        RenderImage(RenderTarget.SVG);
                    }
                } finally {
                    fRenderer.EndDrawing();
                    SetRenderer(prevRenderer);
                }

                return;
            }

            if ((ext == ".bmp" || ext == ".jpg") && imageSize.Width >= 65535) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
            } else {
                ImageFormat imFmt = ImageFormat.Png;
                if (ext == ".bmp") {
                    imFmt = ImageFormat.Bmp;
                } else if (ext == ".png") {
                    imFmt = ImageFormat.Png;
                } else if (ext == ".gif") {
                    imFmt = ImageFormat.Gif;
                } else if (ext == ".jpg") {
                    imFmt = ImageFormat.Jpeg;
                } else if (ext == ".emf") {
                    imFmt = ImageFormat.Emf;
                }

                Image pic;
                if (Equals(imFmt, ImageFormat.Emf)) {
                    using (var gfx = CreateGraphics()) {
                        pic = new Metafile(fileName, gfx.GetHdc());
                    }
                } else {
                    pic = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
                }

                try {
                    using (Graphics gfx = Graphics.FromImage(pic)) {
                        fRenderer.SetTarget(gfx);
                        RenderImage(RenderTarget.RasterFile);
                    }

                    pic.Save(fileName, imFmt);
                } finally {
                    pic.Dispose();
                }
            }
        }

        public virtual void SetRenderer(ChartRenderer renderer)
        {
            fRenderer = renderer;
        }

        #endregion

        #region Navigation support

        private void DoNavRefresh()
        {
            var eventHandler = (EventHandler)Events[EventNavRefresh];
            if (eventHandler != null) eventHandler(this, null);
        }

        protected abstract void SetNavObject(object obj);

        public bool NavAdd(object obj)
        {
            if (obj != null) {
                fNavman.Current = (GDMRecord)obj;
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

            SetNavObject(fNavman.Next());
            DoNavRefresh();
        }

        public void NavPrev()
        {
            if (!fNavman.CanBackward()) return;

            SetNavObject(fNavman.Back());
            DoNavRefresh();
        }

        #endregion
    }
}
