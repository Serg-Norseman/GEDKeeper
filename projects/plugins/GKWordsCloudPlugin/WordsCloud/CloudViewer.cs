/*
 *  "Word Cloud (Tag Cloud)".
 *  Copyright (C) 2011 by George Mamaladze.
 *  http://sourcecodecloud.codeplex.com/
 *  https://www.codeproject.com/Articles/224231/Word-Cloud-Tag-Cloud-Generator-Control-for-NET-Win
 *
 *  This licensed under The Code Project Open License (CPOL).
 *
 *  Adapted for the GEDKeeper project by Sergey V. Zhdanovskih in September 2017.
 */

//#define DEBUG_DRAW

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Text;
using System.Windows.Forms;
using BSLib;
using GKUI.Components;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class CloudViewer : Panel, ICloudRenderer
    {
        private readonly Color[] fPalette = {
            Color.Blue,
            Color.DarkBlue,
            Color.DarkCyan,
            Color.DarkGoldenrod,
            Color.DarkGray,
            Color.DarkGreen,
            Color.DarkKhaki,
            Color.DarkMagenta,
            Color.DarkOrange,
            Color.DarkOrchid,
            Color.DarkRed,
            Color.Green,
            Color.Navy,
            Color.Red,
        };

        private CloudModel fModel;
        private Font fCurrentFont;
        private Graphics fGraphics;
        private FontFamily fFontFamily;
        private float fCurrentSize;

        public CloudViewer()
        {
            BorderStyle = BorderStyle.FixedSingle;
            ResizeRedraw = true;
            fFontFamily = Font.FontFamily;
            fModel = new CloudModel();
        }

        public void SetWeightedWords(List<Word> value)
        {
            if (value == null) return;

            fModel.SetWords(value);
            BuildLayout();
            Invalidate();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fCurrentFont != null) fCurrentFont.Dispose();
                if (fGraphics != null) fGraphics.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitRenderer(Graphics graphics)
        {
            fGraphics = graphics;
            fCurrentFont = null;
            fCurrentSize = 0;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            var gfx = e.Graphics;
            gfx.SmoothingMode = SmoothingMode.AntiAlias;
            gfx.TextRenderingHint = TextRenderingHint.AntiAlias;

            var rect = UIHelper.Rt2Rt(e.ClipRectangle);
            InitRenderer(gfx);
            fModel.Render(this, rect);

            fGraphics = null;
        }

        private void BuildLayout()
        {
            using (var graphics = CreateGraphics()) {
                InitRenderer(graphics);
                var sz = Size;
                fModel.Arrange(this, sz.Width, sz.Height);
            }
            fGraphics = null;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Word nextItemUnderMouse = fModel.GetItemAtLocation(e.X, e.Y);
            if (nextItemUnderMouse != fModel.ItemUnderMouse) {
                if (nextItemUnderMouse != null) {
                    Rectangle newRectangleToInvalidate = RectangleGrow(nextItemUnderMouse.Rectangle, 6);
                    Invalidate(newRectangleToInvalidate);
                }
                if (fModel.ItemUnderMouse != null) {
                    Rectangle prevRectangleToInvalidate = RectangleGrow(fModel.ItemUnderMouse.Rectangle, 6);
                    Invalidate(prevRectangleToInvalidate);
                }
                fModel.ItemUnderMouse = nextItemUnderMouse;
            }
            base.OnMouseMove(e);
        }

        protected override void OnResize(EventArgs eventargs)
        {
            BuildLayout();
            base.OnResize(eventargs);
        }

        private static Rectangle RectangleGrow(ExtRectF original, int growByPixels)
        {
            original.Inflate(-growByPixels, -growByPixels);
            return new Rectangle((int)original.Left, (int)original.Top, (int)original.Width, (int)original.Height);
        }

        ExtSizeF ICloudRenderer.Measure(string text, int weight)
        {
            Font font = GetFont(weight);
            SizeF sz = fGraphics.MeasureString(text, font);
            return new ExtSizeF(sz.Width + 0.5f, sz.Height);
        }

        void ICloudRenderer.Draw(Word word, bool highlight)
        {
            Font font = GetFont(word.Occurrences);
            Color color = fPalette[word.Occurrences * word.Text.Length % fPalette.Length];
            var itemRt = UIHelper.Rt2Rt(word.Rectangle);

            if (highlight) {
                color = UIHelper.Darker(color, 0.5f);
            }

            using (var brush = new SolidBrush(color))
                fGraphics.DrawString(word.Text, font, brush, itemRt);

#if DEBUG_DRAW
            fGraphics.DrawRectangle(new Pen(color), Rectangle.Round(itemRt));
#endif
        }

        private Font GetFont(int weight)
        {
            float fontSize = fModel.GetFontSize(weight);
            if (Math.Abs(fCurrentSize - fontSize) > float.Epsilon) {
                if (fCurrentFont != null) fCurrentFont.Dispose();
                fCurrentFont = new Font(fFontFamily, fontSize, FontStyle.Regular);
            }
            return fCurrentFont;
        }
    }
}
