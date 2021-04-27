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
using System.Drawing;

namespace GKWordsCloudPlugin.WordsCloud
{
    public class Word : IComparable<Word>
    {
        public readonly string Text;
        public readonly int Occurrences;

        public RectangleF Rectangle { get; internal set; }
        public bool IsExposed { get; internal set; }

        public Word(string text, int occurrences)
        {
            Text = text;
            Occurrences = occurrences;
        }

        public int CompareTo(Word other)
        {
            return Occurrences - other.Occurrences;
        }
    }
}
