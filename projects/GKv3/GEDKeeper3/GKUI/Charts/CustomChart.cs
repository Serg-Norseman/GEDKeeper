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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Charts
{
    public abstract class CustomChart : CustomPanel, IPrintable
    {
        private readonly NavigationStack<GEDCOMRecord> fNavman;


        public event EventHandler NavRefresh;


        protected CustomChart() : base()
        {
            CenteredImage = true;

            fNavman = new NavigationStack<GEDCOMRecord>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fNavman != null) fNavman.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            e.Handled = true;
            switch (e.Key) {
                case Keys.Left:
                    AdjustScroll(-SmallChange, 0);
                    break;

                case Keys.Right:
                    AdjustScroll(+SmallChange, 0);
                    break;

                case Keys.Up:
                    AdjustScroll(0, -SmallChange);
                    break;

                case Keys.Down:
                    AdjustScroll(0, +SmallChange);
                    break;

                case Keys.PageUp:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, -LargeChange);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(-LargeChange, 0);
                    }
                    break;

                case Keys.PageDown:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, +LargeChange);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(+LargeChange, 0);
                    }
                    break;

                case Keys.Home:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, -Viewport.Height);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(-Viewport.Width, 0);
                    }
                    break;

                case Keys.End:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, Viewport.Height);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(+Viewport.Width, 0);
                    }
                    break;

                case Keys.Backspace:
                    NavPrev();
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        #region Print and snaphots support

        public abstract ExtSize GetImageSize();
        public abstract void RenderStaticImage(Graphics gfx, OutputType outputType);

        public bool IsLandscape()
        {
            ExtSize imageSize = GetImageSize();
            return (imageSize.Height < imageSize.Width);
        }

        public IImage GetPrintableImage()
        {
            ExtSize imageSize = GetImageSize();
            var frameRect = new Rectangle(0, 0, imageSize.Width, imageSize.Height);

            /*Image image;
            using (var gfx = CreateGraphics()) {
                image = new Metafile(gfx.GetHdc(), frameRect, MetafileFrameUnit.Pixel, EmfType.EmfOnly);
            }
            using (Graphics gfx = Graphics.FromImage(image)) {
                RenderStaticImage(gfx, true);
            }
             */

            var image = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
            using (Graphics gfx = new Graphics(image)) {
                RenderStaticImage(gfx, OutputType.Printer);
            }

            return new ImageHandler(image);
        }

        public virtual void SetSVGMode(bool active, string svgFileName, int width, int height)
        {
            // dummy
        }

        /* TODO(zsv): Need to find an appropriate icon in the general style
         * for the main toolbar - screenshot capture for windows with charts. */
        public void SaveSnapshot(string fileName)
        {
            string ext = SysUtils.GetFileExtension(fileName);

            ExtSize imageSize = GetImageSize();

            if (ext == ".svg") {
                try {
                    SetSVGMode(true, fileName, imageSize.Width, imageSize.Height);

                    using (var gfx = CreateGraphics()) {
                        RenderStaticImage(gfx, OutputType.SVG);
                    }
                } finally {
                    SetSVGMode(false, "", 0, 0);
                }

                return;
            }

            if ((ext == ".bmp" || ext == ".jpg") && imageSize.Width >= 65535) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_TooMuchWidth));
            } else {
                ImageFormat imFmt = ImageFormat.Png;
                if (ext == ".bmp") {
                    imFmt = ImageFormat.Bitmap;
                } else if (ext == ".png") {
                    imFmt = ImageFormat.Png;
                } else if (ext == ".gif") {
                    imFmt = ImageFormat.Gif;
                } else if (ext == ".jpg") {
                    imFmt = ImageFormat.Jpeg;
                }
                /*else
                    if (ext == ".emf") { imFmt = ImageFormat.Emf; }*/

                /*Image pic;
                if (Equals(imFmt, ImageFormat.Emf)) {
                    using (var gfx = CreateGraphics()) {
                        pic = new Metafile(fileName, gfx.GetHdc());
                    }
                } else {
                    pic = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
                }*/

                Bitmap pic = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
                try {
                    //using (Graphics gfx = Graphics.FromImage(pic)) {
                    using (Graphics gfx = new Graphics(pic)) {
                        RenderStaticImage(gfx, OutputType.StdFile);
                    }

                    ((Bitmap)pic).Save(fileName, imFmt);
                } finally {
                    pic.Dispose();
                }
            }
        }

        #endregion

        #region Navigation support

        private void DoNavRefresh()
        {
            var eventHandler = (EventHandler)NavRefresh;
            if (eventHandler != null) eventHandler(this, null);
        }

        protected abstract void SetNavObject(object obj);

        public bool NavAdd(object obj)
        {
            if (obj != null && !fNavman.Busy) {
                fNavman.Current = (GEDCOMRecord)obj;
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
