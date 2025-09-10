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
using System.Linq;
using BSLib;

namespace GKWordsCloudPlugin.WordsCloud
{
    public interface ICloudRenderer : IDisposable
    {
        ExtSizeF Measure(string text, int weight);
        void Draw(Word word, bool highlight);
    }

    public class CloudModel
    {
        private ExtPointF fCenter;
        private QuadTree<Word> fQuadTree;
        private ExtRectF fSurface;
        private List<Word> fWords;

        private int fMaxFontSize;
        private int fMinFontSize;
        private int fMaxWordWeight;
        private int fMinWordWeight;

        public Word ItemUnderMouse { get; set; }
        public int MaxWordWeight { get { return fMaxWordWeight; } }
        public int MinWordWeight { get { return fMinWordWeight; } }


        public CloudModel()
        {
            fMaxFontSize = 40;
            fMinFontSize = 6;

            fMinWordWeight = 0;
            fMaxWordWeight = 0;
        }

        public float GetFontSize(int weight)
        {
            float fontSize = (float)(weight - fMinWordWeight) / (fMaxWordWeight - fMinWordWeight) * (fMaxFontSize - fMinFontSize) + fMinFontSize;
            return fontSize;
        }

        public void Render(ICloudRenderer renderer, ExtRectF area)
        {
            var wordsToRedraw = GetWordsInArea(area);
            foreach (Word word in wordsToRedraw) {
                if (word.IsExposed) {
                    renderer.Draw(word, (ItemUnderMouse == word));
                }
            }
        }

        public void SetWords(List<Word> words)
        {
            if (words == null)
                throw new ArgumentNullException("words");

            fWords = words;
        }

        public void Arrange(ICloudRenderer renderer, float sizeWidth, float sizeHeight)
        {
            if (fWords == null) return;

            fSurface = new ExtRectF(0, 0, sizeWidth, sizeHeight);
            fQuadTree = new QuadTree<Word>(fSurface);
            fCenter = new ExtPointF(fSurface.Left + sizeWidth / 2, fSurface.Top + sizeHeight / 2);

            Word first = fWords.FirstOrDefault();
            if (first != null) {
                fMaxWordWeight = first.Occurrences;
                fMinWordWeight = fWords.Last().Occurrences;
            }

            foreach (Word word in fWords) {
                var size = renderer.Measure(word.Text, word.Occurrences);

                if (TryFindFreeRectangle(size, out ExtRectF freeRectangle)) {
                    word.Rectangle = freeRectangle;
                    word.IsExposed = true;
                    fQuadTree.Insert(word);
                }
            }
        }

        public IEnumerable<Word> GetWordsInArea(ExtRectF area)
        {
            return (fQuadTree == null) ? new List<Word>() : fQuadTree.Query(area);
        }

        public Word GetItemAtLocation(int locX, int locY)
        {
            var area = new ExtRectF(locX, locY, 0, 0);
            return GetWordsInArea(area).FirstOrDefault();
        }

        private bool IsInsideSurface(ExtRectF target)
        {
            return target.Left >= fSurface.Left && target.Top >= fSurface.Top && target.Bottom <= fSurface.Bottom && target.Right <= fSurface.Right;
        }

        public bool TryFindFreeRectangle(ExtSizeF size, out ExtRectF foundRectangle)
        {
            foundRectangle = ExtRectF.Empty;
            double alpha = GetStartPseudoAngle(size);
            const double stepAlpha = Math.PI / 60;

            const double pointsOnSpital = 500;

            for (int pointIndex = 0; pointIndex < pointsOnSpital; pointIndex++) {
                double dX = pointIndex / pointsOnSpital * Math.Sin(alpha) * fCenter.X;
                double dY = pointIndex / pointsOnSpital * Math.Cos(alpha) * fCenter.Y;
                foundRectangle = new ExtRectF((float)(fCenter.X + dX) - size.Width / 2.0f, (float)(fCenter.Y + dY) - size.Height / 2.0f, size.Width, size.Height);

                alpha += stepAlpha;
                if (!IsInsideSurface(foundRectangle)) {
                    return false;
                }

                if (!fQuadTree.HasContent(foundRectangle)) {
                    return true;
                }
            }

            return false;
        }

        private static float GetStartPseudoAngle(ExtSizeF size)
        {
            return size.Height * size.Width;
        }
    }
}
