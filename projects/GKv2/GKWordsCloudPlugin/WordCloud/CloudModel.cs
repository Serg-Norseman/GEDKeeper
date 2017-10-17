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

namespace WordCloud
{
    public interface ICloudRenderer : IDisposable
    {
        SizeF Measure(string text, int weight);
        void Draw(Word word, bool highlight);
    }

    public class CloudModel
    {
        private readonly PointF fCenter;
        private readonly QuadTree<Word> fQuadTree;
        private readonly RectangleF fSurface;

        public CloudModel(SizeF size)
        {
            fSurface = new RectangleF(new PointF(0, 0), size);
            fQuadTree = new QuadTree<Word>(fSurface);
            fCenter = new PointF(fSurface.X + size.Width / 2, fSurface.Y + size.Height / 2);
        }

        public void Arrange(List<Word> words, ICloudRenderer renderer)
        {
            if (words == null) {
                throw new ArgumentNullException("words");
            }

            foreach (Word word in words) {
                SizeF size = renderer.Measure(word.Text, word.Occurrences);

                RectangleF freeRectangle;
                if (TryFindFreeRectangle(size, out freeRectangle)) {
                    word.Rectangle = freeRectangle;
                    word.IsExposed = true;
                    fQuadTree.Insert(word);
                }
            }
        }

        public IEnumerable<Word> GetWordsInArea(RectangleF area)
        {
            return fQuadTree.Query(area);
        }

        private bool IsInsideSurface(RectangleF target)
        {
            return target.X >= fSurface.X && target.Y >= fSurface.Y &&
            target.Bottom <= fSurface.Bottom && target.Right <= fSurface.Right;
        }

        public bool TryFindFreeRectangle(SizeF size, out RectangleF foundRectangle)
        {
            foundRectangle = RectangleF.Empty;
            double alpha = GetStartPseudoAngle(size);
            const double stepAlpha = Math.PI / 60;

            const double pointsOnSpital = 500;

            Math.Min(fCenter.Y, fCenter.X);
            for (int pointIndex = 0; pointIndex < pointsOnSpital; pointIndex++) {
                double dX = pointIndex / pointsOnSpital * Math.Sin(alpha) * fCenter.X;
                double dY = pointIndex / pointsOnSpital * Math.Cos(alpha) * fCenter.Y;
                foundRectangle = new RectangleF((float)(fCenter.X + dX) - size.Width / 2, (float)(fCenter.Y + dY) - size.Height / 2, size.Width, size.Height);

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

        private static float GetStartPseudoAngle(SizeF size)
        {
            return size.Height * size.Width;
        }
    }
}
