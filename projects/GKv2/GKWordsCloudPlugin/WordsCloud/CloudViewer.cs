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

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Text;
using System.Linq;
using System.Windows.Forms;
using BSLib;
using GKUI.Components;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class CloudViewer : Panel, ICloudRenderer
    {
        private readonly Color[] fPalette = {
            Color.DarkRed,
            Color.DarkBlue,
            Color.DarkGreen,
            Color.Navy,
            Color.DarkCyan,
            Color.DarkOrange,
            Color.DarkGoldenrod,
            Color.DarkKhaki,
            Color.Blue,
            Color.Red,
            Color.Green
        };

        private Color fBackColor;
        private Word fItemUnderMouse;
        private int fMaxFontSize;
        private int fMinFontSize;
        private int fMaxWordWeight;
        private int fMinWordWeight;
        private CloudModel fModel;
        private List<Word> fWords;

        private Font fCurrentFont;
        private Graphics fGraphics;
        private FontFamily fFontFamily;
        private FontStyle fFontStyle;

        public override Color BackColor
        {
            get {
                return fBackColor;
            }
            set {
                if (fBackColor == value) {
                    return;
                }
                fBackColor = value;
                Invalidate();
            }
        }

        public int MaxFontSize
        {
            get { return fMaxFontSize; }
            set {
                fMaxFontSize = value;
                BuildLayout();
                Invalidate();
            }
        }

        public int MinFontSize
        {
            get { return fMinFontSize; }
            set {
                fMinFontSize = value;
                BuildLayout();
                Invalidate();
            }
        }

        public List<Word> WeightedWords
        {
            get { return fWords; }
            set {
                fWords = value;
                if (value == null) {
                    return;
                }

                Word first = fWords.FirstOrDefault();
                if (first != null) {
                    fMaxWordWeight = first.Occurrences;
                    fMinWordWeight = fWords.Last().Occurrences;
                }

                BuildLayout();
                Invalidate();
            }
        }

        public CloudViewer()
        {
            BorderStyle = BorderStyle.FixedSingle;
            ResizeRedraw = true;

            fBackColor = Color.White;
            fMinWordWeight = 0;
            fMaxWordWeight = 0;

            MaxFontSize = 68;
            MinFontSize = 6;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fCurrentFont != null) fCurrentFont.Dispose();
                if (fGraphics != null) fGraphics.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitRenderer(Graphics graphics, FontFamily fontFamily, FontStyle fontStyle)
        {
            fGraphics = graphics;
            fFontFamily = fontFamily;
            fFontStyle = fontStyle;
            fCurrentFont = new Font(fFontFamily, fMaxFontSize, fFontStyle);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            if (fWords == null || fModel == null) {
                return;
            }

            IEnumerable<Word> wordsToRedraw = fModel.GetWordsInArea(e.ClipRectangle);

            var gfx = e.Graphics;
            gfx.SmoothingMode = SmoothingMode.AntiAlias;
            gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
            InitRenderer(gfx, Font.FontFamily, FontStyle.Regular);
            foreach (Word word in wordsToRedraw) {
                if (word.IsExposed) {
                    Draw(word, (fItemUnderMouse == word));
                }
            }
            fGraphics = null;
        }

        private void BuildLayout()
        {
            if (fWords == null) {
                return;
            }

            using (Graphics graphics = CreateGraphics()) {
                InitRenderer(graphics, Font.FontFamily, FontStyle.Regular);

                fModel = new CloudModel(Size);
                fModel.Arrange(fWords, this);
            }
            fGraphics = null;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Word nextItemUnderMouse = GetItemAtLocation(e.Location);
            if (nextItemUnderMouse != fItemUnderMouse) {
                if (nextItemUnderMouse != null) {
                    Rectangle newRectangleToInvalidate = RectangleGrow(nextItemUnderMouse.Rectangle, 6);
                    Invalidate(newRectangleToInvalidate);
                }
                if (fItemUnderMouse != null) {
                    Rectangle prevRectangleToInvalidate = RectangleGrow(fItemUnderMouse.Rectangle, 6);
                    Invalidate(prevRectangleToInvalidate);
                }
                fItemUnderMouse = nextItemUnderMouse;
            }
            base.OnMouseMove(e);
        }

        protected override void OnResize(EventArgs eventargs)
        {
            BuildLayout();
            base.OnResize(eventargs);
        }

        private static Rectangle RectangleGrow(RectangleF original, int growByPixels)
        {
            return new Rectangle(
                (int)(original.X - growByPixels),
                (int)(original.Y - growByPixels),
                (int)(original.Width + growByPixels + 1),
                (int)(original.Height + growByPixels + 1));
        }

        public Word GetItemAtLocation(Point location)
        {
            var area = new RectangleF(location, new SizeF(0, 0));
            IEnumerable<Word> itemsInArea = (fModel == null) ? new Word[] { } : fModel.GetWordsInArea(area);
            return itemsInArea.FirstOrDefault();
        }

        public SizeF Measure(string text, int weight)
        {
            Font font = GetFont(weight);
            Size proposedSize = new Size(int.MaxValue, int.MaxValue);
            return fGraphics.MeasureString(text, font, proposedSize, StringFormat.GenericTypographic);
        }

        public void Draw(Word word, bool highlight)
        {
            Font font = GetFont(word.Occurrences);
            Color color = fPalette[word.Occurrences * word.Text.Length % fPalette.Length];
            RectangleF itemRt = word.Rectangle;

            if (highlight) {
                color = UIHelper.Darker(color, 0.5f);
            }

            Brush brush = new SolidBrush(color);
            fGraphics.DrawString(word.Text, font, brush, itemRt, StringFormat.GenericTypographic);

#if DEBUG_DRAW
            fGraphics.DrawRectangle(new Pen(color), Rectangle.Round(itemRt));
#endif
        }

        private Font GetFont(int weight)
        {
            float fontSize = (float)(weight - fMinWordWeight) / (fMaxWordWeight - fMinWordWeight) * (fMaxFontSize - fMinFontSize) + fMinFontSize;
            if (Math.Abs(fCurrentFont.Size - fontSize) > float.Epsilon) {
                if (fCurrentFont != null) fCurrentFont.Dispose();
                fCurrentFont = new Font(fFontFamily, fontSize, fFontStyle);
            }
            return fCurrentFont;
        }
    }
}
