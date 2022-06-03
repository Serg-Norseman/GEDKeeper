﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Charts;
using GKUI.Platform;

namespace GKUI.Components
{
    public abstract class CustomChart : ScrollablePanel, IPrintable
    {
        private readonly NavigationStack<GDMRecord> fNavman;
        protected ChartRenderer fRenderer;


        public Image BackgroundImage { get; set; }

        public event EventHandler NavRefresh;


        protected CustomChart()
        {
            CenteredImage = true;

            fNavman = new NavigationStack<GDMRecord>();
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.Key) {
                case Keys.Keypad4:
                case Keys.Left:
                    AdjustScroll(-SmallChange, 0);
                    e.Handled = true;
                    break;

                case Keys.Keypad6:
                case Keys.Right:
                    AdjustScroll(+SmallChange, 0);
                    e.Handled = true;
                    break;

                case Keys.Keypad8:
                case Keys.Up:
                    AdjustScroll(0, -SmallChange);
                    e.Handled = true;
                    break;

                case Keys.Keypad2:
                case Keys.Down:
                    AdjustScroll(0, +SmallChange);
                    e.Handled = true;
                    break;

                case Keys.Keypad9:
                case Keys.PageUp:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, -LargeChange);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(-LargeChange, 0);
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad3:
                case Keys.PageDown:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, +LargeChange);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(+LargeChange, 0);
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad7:
                case Keys.Home:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, -Viewport.Height);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(-Viewport.Width, 0);
                    }
                    e.Handled = true;
                    break;

                case Keys.Keypad1:
                case Keys.End:
                    if (Keys.None == e.Modifiers) {
                        AdjustScroll(0, Viewport.Height);
                    } else if (Keys.Shift == e.Modifiers) {
                        AdjustScroll(+Viewport.Width, 0);
                    }
                    e.Handled = true;
                    break;

                case Keys.Backspace:
                    NavPrev();
                    e.Handled = true;
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        #region Print and snaphots support

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

            var image = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
            using (Graphics gfx = new Graphics(image)) {
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
                    imFmt = ImageFormat.Bitmap;
                } else if (ext == ".png") {
                    imFmt = ImageFormat.Png;
                } else if (ext == ".gif") {
                    imFmt = ImageFormat.Gif;
                } else if (ext == ".jpg") {
                    imFmt = ImageFormat.Jpeg;
                } else if (ext == ".emf") {
                    /* Emf is not supported */
                }


                Bitmap pic = new Bitmap(imageSize.Width, imageSize.Height, PixelFormat.Format24bppRgb);
                try {
                    using (Graphics gfx = new Graphics(pic)) {
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
            var eventHandler = NavRefresh;
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
