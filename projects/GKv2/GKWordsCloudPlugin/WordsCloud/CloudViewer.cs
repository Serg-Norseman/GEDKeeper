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
using System.Linq;
using System.Windows.Forms;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class CloudViewer : Panel
    {
        private readonly Color[] fDefaultPalette = {
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
        private Color[] fPalette;
        private List<Word> fWords;

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

        public Color[] Palette
        {
            get { return fPalette; }
            set {
                fPalette = value;
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
            fPalette = fDefaultPalette;

            MaxFontSize = 68;
            MinFontSize = 6;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            if (fWords == null || fModel == null) {
                return;
            }

            IEnumerable<Word> wordsToRedraw = fModel.GetWordsInArea(e.ClipRectangle);
            using (Graphics graphics = e.Graphics)
                using (ICloudRenderer graphicEngine =
                       new GdiRenderer(graphics, Font.FontFamily, FontStyle.Regular, fPalette, MinFontSize, MaxFontSize, fMinWordWeight, fMaxWordWeight)) {
                foreach (Word word in wordsToRedraw) {
                    if (word.IsExposed) {
                        graphicEngine.Draw(word, (fItemUnderMouse == word));
                    }
                }
            }
        }

        private void BuildLayout()
        {
            if (fWords == null) {
                return;
            }

            using (Graphics graphics = CreateGraphics()) {
                ICloudRenderer graphicEngine =
                    new GdiRenderer(graphics, Font.FontFamily, FontStyle.Regular, fPalette, MinFontSize, MaxFontSize, fMinWordWeight, fMaxWordWeight);

                fModel = new CloudModel(Size);
                fModel.Arrange(fWords, graphicEngine);
            }
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

        public IEnumerable<Word> GetItemsInArea(RectangleF area)
        {
            return (fModel == null) ? new Word[] { } : fModel.GetWordsInArea(area);
        }

        public Word GetItemAtLocation(Point location)
        {
            IEnumerable<Word> itemsInArea = GetItemsInArea(new RectangleF(location, new SizeF(0, 0)));
            return itemsInArea.FirstOrDefault();
        }
    }
}
