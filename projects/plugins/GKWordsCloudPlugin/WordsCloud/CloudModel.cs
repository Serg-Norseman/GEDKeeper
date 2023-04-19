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
        private readonly ExtPointF fCenter;
        private readonly QuadTree<Word> fQuadTree;
        private readonly ExtRectF fSurface;

        public CloudModel(float sizeWidth, float sizeHeight)
        {
            fSurface = new ExtRectF(0, 0, sizeWidth, sizeHeight);
            fQuadTree = new QuadTree<Word>(fSurface);
            fCenter = new ExtPointF(fSurface.Left + sizeWidth / 2, fSurface.Top + sizeHeight / 2);
        }

        public void Arrange(List<Word> words, ICloudRenderer renderer)
        {
            if (words == null) {
                throw new ArgumentNullException("words");
            }

            foreach (Word word in words) {
                var size = renderer.Measure(word.Text, word.Occurrences);

                ExtRectF freeRectangle;
                if (TryFindFreeRectangle(size, out freeRectangle)) {
                    word.Rectangle = freeRectangle;
                    word.IsExposed = true;
                    fQuadTree.Insert(word);
                }
            }
        }

        public IEnumerable<Word> GetWordsInArea(ExtRectF area)
        {
            return fQuadTree.Query(area);
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
