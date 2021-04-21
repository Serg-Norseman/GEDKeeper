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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Text;
using System.Windows.Forms;
using GKUI.Components;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class GdiRenderer : ICloudRenderer
    {
        private const TextFormatFlags FLAGS = TextFormatFlags.NoPadding;

        private Font fCurrentFont;
        private readonly Graphics fGraphics;
        private readonly int fMaxWordWeight;
        private readonly int fMinWordWeight;

        public FontFamily FontFamily { get; set; }
        public FontStyle FontStyle { get; set; }
        public Color[] Palette { get; private set; }
        public float MinFontSize { get; set; }
        public float MaxFontSize { get; set; }

        public GdiRenderer(Graphics graphics, FontFamily fontFamily, FontStyle fontStyle, Color[] palette, float minFontSize, float maxFontSize, int minWordWeight, int maxWordWeight)
        {
            fMinWordWeight = minWordWeight;
            fMaxWordWeight = maxWordWeight;
            fGraphics = graphics;
            FontFamily = fontFamily;
            FontStyle = fontStyle;
            Palette = palette;
            MinFontSize = minFontSize;
            MaxFontSize = maxFontSize;
            fCurrentFont = new Font(FontFamily, maxFontSize, FontStyle);
            fGraphics.SmoothingMode = SmoothingMode.AntiAlias;
            fGraphics.TextRenderingHint = TextRenderingHint.AntiAlias;
        }

        public SizeF Measure(string text, int weight)
        {
            Font font = GetFont(weight);

            Size proposedSize = new Size(int.MaxValue, int.MaxValue);
            //return TextRenderer.MeasureText(_graphics, text, font, proposedSize, _flags);

            return fGraphics.MeasureString(text, font, proposedSize, StringFormat.GenericTypographic);
            //return TextRenderer.MeasureText(_graphics, text, font);
        }

        public void Draw(Word word, bool highlight)
        {
            Font font = GetFont(word.Occurrences);
            Color color = GetPresudoRandomColorFromPalette(word);
            RectangleF itemRt = word.Rectangle;

            if (highlight) {
                color = UIHelper.Darker(color, 0.5f);
            }

            Brush brush = new SolidBrush(color);
            fGraphics.DrawString(word.Text, font, brush, itemRt, StringFormat.GenericTypographic);

            #if DEBUG_DRAW
            fGraphics.DrawRectangle(new Pen(color), Rectangle.Round(itemRt));
            #endif

            //TextRenderer.DrawText(_graphics, word.Text, font, point, color, FLAGS);
        }

        private Font GetFont(int weight)
        {
            float fontSize = (float)(weight - fMinWordWeight) / (fMaxWordWeight - fMinWordWeight) * (MaxFontSize - MinFontSize) + MinFontSize;
            if (Math.Abs(fCurrentFont.Size - fontSize) > float.Epsilon) {
                SetFont(new Font(FontFamily, fontSize, FontStyle));
            }
            return fCurrentFont;
        }

        private void SetFont(Font font)
        {
            if (fCurrentFont != null) fCurrentFont.Dispose();
            fCurrentFont = font;
        }

        private Color GetPresudoRandomColorFromPalette(Word word)
        {
            Color color = Palette[word.Occurrences * word.Text.Length % Palette.Length];
            return color;
        }

        public void Dispose()
        {
            if (fCurrentFont != null) fCurrentFont.Dispose();
            fGraphics.Dispose();
        }
    }
}
