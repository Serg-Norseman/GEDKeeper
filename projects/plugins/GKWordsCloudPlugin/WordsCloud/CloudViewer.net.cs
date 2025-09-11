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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class CloudViewer : Drawable, ICloudRenderer
    {
        private readonly Color[] fPalette = {
            Colors.Blue,
            Colors.DarkBlue,
            Colors.DarkCyan,
            Colors.DarkGoldenrod,
            Colors.DarkGray,
            Colors.DarkGreen,
            Colors.DarkKhaki,
            Colors.DarkMagenta,
            Colors.DarkOrange,
            Colors.DarkOrchid,
            Colors.DarkRed,
            Colors.Green,
            Colors.Navy,
            Colors.Red,
        };

        private readonly CloudModel fModel;
        private Font fCurrentFont;
        private Graphics fGraphics;
        private float fCurrentSize;

        public CloudViewer()
        {
            fModel = new CloudModel(this);
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
            gfx.AntiAlias = true;

            var rect = UIHelper.Rt2Rt(e.ClipRectangle);
            InitRenderer(gfx);
            fModel.Render(rect);
        }

        private void BuildLayout()
        {
            {
                InitRenderer(null);
                var sz = ClientSize;
                fModel.Arrange(sz.Width, sz.Height);
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Word nextItemUnderMouse = fModel.GetItemAtLocation((int)e.Location.X, (int)e.Location.Y);
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

        protected override void OnSizeChanged(EventArgs eventargs)
        {
            BuildLayout();
            base.OnSizeChanged(eventargs);
        }

        private static Rectangle RectangleGrow(ExtRectF original, int growByPixels)
        {
            original.Inflate(-growByPixels, -growByPixels);
            return new Rectangle((int)original.Left, (int)original.Top, (int)original.Width, (int)original.Height);
        }

        ExtSizeF ICloudRenderer.Measure(string text, int weight)
        {
            Font font = GetFont(weight);
            SizeF sz = font.MeasureString(text);
            return new ExtSizeF(sz.Width + 2.0f, sz.Height + 0.5f);
        }

        void ICloudRenderer.Draw(Word word, bool highlight)
        {
            Font font = GetFont(word.Occurrences);
            Color color = fPalette[word.Occurrences * word.Text.Length % fPalette.Length];
            var itemRt = Rectangle.Round(UIHelper.Rt2Rt(word.Rectangle));

            if (highlight) {
                color = UIHelper.Darker(color, 0.5f);
            }

            using (var brush = new SolidBrush(color))
                fGraphics.DrawText(font, brush, itemRt, word.Text);

#if DEBUG_DRAW
            fGraphics.DrawRectangle(new Pen(color), itemRt);
#endif
        }

        private Font GetFont(int weight)
        {
            float fontSize = fModel.GetFontSize(weight);
            if (Math.Abs(fCurrentSize - fontSize) > float.Epsilon) {
                fCurrentSize = fontSize;
                if (fCurrentFont != null) fCurrentFont.Dispose();
                fCurrentFont = new Font(FontFamilies.Sans, fontSize, FontStyle.None);
            }
            return fCurrentFont;
        }
    }
}
