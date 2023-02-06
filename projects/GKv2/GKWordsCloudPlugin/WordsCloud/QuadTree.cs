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

using System.Collections.Generic;
using System.Drawing;

namespace GKWordsCloudPlugin.WordsCloud
{
    /// <summary>
    ///   A Quadtree is a structure designed to partition space so
    ///   that it's faster to find out what is inside or outside a given
    ///   area. See http://en.wikipedia.org/wiki/Quadtree
    ///   This QuadTree contains items that have an area (RectangleF)
    ///   it will store a reference to the item in the quad
    ///   that is just big enough to hold it. Each quad has a bucket that
    ///   contain multiple items.
    /// </summary>
    public class QuadTree<T> where T : Word
    {
        public delegate void QuadTreeAction(QuadTreeNode<T> obj);

        private readonly QuadTreeNode<T> fRoot;

        public QuadTree(RectangleF rectangle)
        {
            fRoot = new QuadTreeNode<T>(rectangle);
        }

        public int Count
        {
            get { return fRoot.Count; }
        }

        public void Insert(T item)
        {
            fRoot.Insert(item);
        }

        public IEnumerable<T> Query(RectangleF area)
        {
            return fRoot.Query(area);
        }

        public bool HasContent(RectangleF area)
        {
            return fRoot.HasContent(area);
        }

        public void ForEach(QuadTreeAction action)
        {
            fRoot.ForEach(action);
        }
    }
}
