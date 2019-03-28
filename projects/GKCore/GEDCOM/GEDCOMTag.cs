/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using BSLib;
using GKCore;

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// The main class of the entire storage infrastructure in the GEDCOM format.
    /// The ancestor of all structural classes. Contains the main logic
    /// for reading and writing the values of tags in the terminology and
    /// according to the rules of GEDCOM.
    /// </summary>
    public class GEDCOMTag : GEDCOMObject
    {
        #region Protected fields

        private int fLevel;
        private string fName;
        private GEDCOMObject fOwner;
        protected string fStringValue;
        private GEDCOMList<GEDCOMTag> fTags;

        #endregion

        #region Public properties

        /// <summary>
        /// The number of nested sub-tags.
        /// </summary>
        public int Count
        {
            get { return fTags.Count; }
        }

        /// <summary>
        /// Access to items of nested sub-tags.
        /// </summary>
        public GEDCOMTag this[int index]
        {
            get { return fTags[index]; }
        }

        internal int Level
        {
            get { return fLevel; }
        }

        public string Name
        {
            get { return fName; }
        }

        public GEDCOMObject Owner
        {
            get { return fOwner; }
        }

        public string StringValue
        {
            get { return GetStringValue(); }
            set { ParseString(value); }
        }

        #endregion

        #region Object management

        public static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMTag(owner, tagName, tagValue);
        }

        public GEDCOMTag(GEDCOMObject owner)
        {
            fOwner = owner;
            fTags = new GEDCOMList<GEDCOMTag>(this);
            fStringValue = string.Empty;

            GEDCOMTag ownerTag = owner as GEDCOMTag;
            fLevel = (ownerTag != null) ? ownerTag.Level + 1 : 0;
        }

        public GEDCOMTag(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTags != null) {
                    fTags.Dispose();
                    fTags = null;
                }
            }
            base.Dispose(disposing);
        }

        protected GEDCOMList<GEDCOMTag> GetTagList()
        {
            return fTags;
        }

        public void SetNameValue(string tagName, string tagValue)
        {
            if (!string.IsNullOrEmpty(tagName)) {
                SetName(tagName);
            }

            if (!string.IsNullOrEmpty(tagValue)) {
                ParseString(tagValue);
            }
        }

        #endregion

        #region Content management

        public virtual GEDCOMTree GetTree()
        {
            GEDCOMTree owner = null;

            GEDCOMTag current = this;
            while (current != null) {
                GEDCOMObject parent = current.fOwner;

                var parentTag = parent as GEDCOMTag;
                if (parentTag != null) {
                    current = parentTag;
                } else {
                    var parentTree = parent as GEDCOMTree;
                    if (parentTree != null) {
                        owner = parentTree;
                    }
                    break;
                }
            }

            return owner;
        }

        protected GEDCOMRecord FindRecord(string xref)
        {
            GEDCOMTree tree = GetTree();
            return (tree == null) ? null : tree.XRefIndex_Find(xref);
        }

        protected GEDCOMTag InsertTag(GEDCOMTag tag)
        {
            fTags.Add(tag);
            return tag;
        }

        public bool IsEmptySkip()
        {
            TagProperties props = GEDCOMProvider.GetTagProps(fName);
            return (props != null && props.SkipEmpty);
        }

        internal void SetLevel(int value)
        {
            fLevel = value;
        }

        public void SetName(string value)
        {
            fName = value;
        }

        /// <summary>
        /// Adding nested sub-tags.
        /// </summary>
        /// <param name="tagName">A name of sub-tag.</param>
        /// <param name="tagValue">A string value of sub-tag.</param>
        /// <param name="tagConstructor">The default constructor of sub-tag.</param>
        /// <returns></returns>
        public virtual GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GEDCOMTag tag = null;
            try {
                if (tagConstructor != null) {
                    tag = tagConstructor(this, tagName, tagValue);
                } else {
                    tag = GEDCOMFactory.GetInstance().CreateTag(this, tagName, tagValue);
                    if (tag == null) {
                        tag = new GEDCOMTag(this, tagName, tagValue);
                    }
                }

                InsertTag(tag);
            } catch (Exception ex) {
                Logger.LogWrite("GEDCOMTag.AddTag(): " + ex.Message);
            }
            return tag;
        }

        /// <summary>
        /// Copying the sub-tags from the source to the current tag.
        /// </summary>
        /// <param name="source">A source tag.</param>
        public virtual void Assign(GEDCOMTag source/*, string[] skipList = null*/)
        {
            if (source == null) return;

            SetName(source.Name);
            ParseString(source.StringValue);

            foreach (GEDCOMTag sourceTag in source.fTags) {
                GEDCOMTag copy = CreateCopy(sourceTag);
                InsertTag(copy);
            }
        }

        protected void AssignList(GEDCOMList<GEDCOMTag> srcList, GEDCOMList<GEDCOMTag> destList)
        {
            foreach (GEDCOMTag sourceTag in srcList) {
                GEDCOMTag copy = CreateCopy(sourceTag);
                destList.Add(copy);
            }
        }

        private GEDCOMTag CreateCopy(GEDCOMTag sourceTag)
        {
            GEDCOMTag result = (GEDCOMTag)Activator.CreateInstance(sourceTag.GetType(), new object[] { this, string.Empty, string.Empty });
            result.Assign(sourceTag);
            return result;
        }

        public virtual void Clear()
        {
            fTags.Clear();
            fStringValue = string.Empty;
        }

        public void Delete(int index)
        {
            fTags.DeleteAt(index);
        }

        public void DeleteTag(string tagName)
        {
            GEDCOMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                fTags.DeleteAt(idx);

                tag = FindTag(tagName, idx);
            }
        }

        public GEDCOMTag FindTag(string tagName, int startIndex)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(tagName);

            int pos = su.IndexOf('\\');
            string S = ((pos >= 0) ? su.Substring(0, pos) : su);

            GEDCOMTag tempTag = this;

            while (true) {
                int index = ((S == su) ? startIndex : 0);

                GEDCOMList<GEDCOMTag> tempSubTags = tempTag.fTags;
                int tempSubCount = tempSubTags.Count;
                while (index < tempSubCount && tempSubTags[index].Name != S) index++;
                if (index >= tempSubCount) break;
                tempTag = tempSubTags[index];

                pos = su.IndexOf('\\');
                if (pos >= 0) {
                    su = su.Substring(pos + 1);

                    pos = su.IndexOf('\\');
                    S = ((pos >= 0) ? su.Substring(0, pos) : su);
                } else {
                    su = "";
                }

                if (su == "") return tempTag;
            }

            return null;
        }

        public IList<GEDCOMTag> FindTags(string tagName)
        {
            IList<GEDCOMTag> result = new List<GEDCOMTag>();

            GEDCOMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                result.Add(tag);

                tag = FindTag(tagName, idx + 1);
            }

            return result;
        }

        public GEDCOMTag TagClass(string tagName, TagConstructor tagConstructor)
        {
            GEDCOMTag result = FindTag(tagName, 0);

            if (result == null) {
                result = AddTag(tagName, "", tagConstructor);
            }

            return result;
        }

        public int IndexOfTag(GEDCOMTag tag)
        {
            return fTags.IndexOf(tag);
        }

        public virtual bool IsEmpty()
        {
            return (string.IsNullOrEmpty(fStringValue) && (fTags.Count == 0));
        }

        public virtual float IsMatch(GEDCOMTag tag, MatchParams matchParams)
        {
            return 0.0f;
        }

        protected virtual float GetStrMatch(string str1, string str2, MatchParams matchParams)
        {
            float match = 0.0f;

            if (matchParams.NamesIndistinctThreshold >= 0.99f) {
                if (string.Compare(str1, str2, true) == 0) {
                    match = 100.0f;
                }
            } else {
                double sim = IndistinctMatching.GetSimilarity(str1, str2);
                if (sim >= matchParams.NamesIndistinctThreshold) {
                    match = 100.0f;
                }
            }

            return match;
        }

        #endregion

        #region Values management

        protected virtual string GetStringValue()
        {
            return fStringValue;
        }

        public virtual string ParseString(string strValue)
        {
            fStringValue = strValue;
            return string.Empty;
        }


        public int GetTagIntegerValue(string tagName, int defValue)
        {
            string str = GetTagStringValue(tagName);
            int result = ((str == "") ? defValue : ConvertHelper.ParseInt(str, defValue));
            return result;
        }

        public void SetTagIntegerValue(string tagName, int value)
        {
            SetTagStringValue(tagName, value.ToString());
        }


        public double GetTagFloatValue(string tagName, double defValue)
        {
            string str = GetTagStringValue(tagName);
            double result = ((str == "") ? defValue : ConvertHelper.ParseFloat(str, defValue));
            return result;
        }

        public void SetTagFloatValue(string tagName, double value)
        {
            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";
            SetTagStringValue(tagName, value.ToString(nfi));
        }


        public string GetTagStringValue(string tagName)
        {
            GEDCOMTag tag = FindTag(tagName, 0);
            string result = ((tag == null) ? "" : tag.StringValue);
            return result;
        }

        public void SetTagStringValue(string tagName, string value)
        {
            string su = tagName;

            GEDCOMTag P = FindTag(su, 0);

            if (P != null) {
                P.StringValue = value;
            } else {
                GEDCOMTag O = this;
                while (su != "") {
                    string S;

                    int index = su.IndexOf('\\');
                    if (index >= 0) {
                        S = su.Substring(0, index);
                        su = su.Substring(index + 1);
                    } else {
                        S = su;
                        su = "";
                    }

                    P = O.FindTag(S, 0);
                    if (P == null) {
                        if (su == "") {
                            P = O.AddTag(S, value, null);
                        } else {
                            P = O.AddTag(S, "", null);
                        }
                    } else {
                        if (su == "") {
                            P.StringValue = value;
                        }
                    }
                    O = P;
                }
            }
        }


        public static StringList GetTagStrings(GEDCOMTag strTag)
        {
            StringList strings = new StringList();

            if (strTag != null) {
                if (strTag.StringValue != "") {
                    strings.Add(strTag.StringValue);
                }

                int num = strTag.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMTag tag = strTag[i];

                    if (tag.Name == "CONC") {
                        if (strings.Count > 0) {
                            strings[strings.Count - 1] = strings[strings.Count - 1] + tag.StringValue;
                        } else {
                            strings.Add(tag.StringValue);
                        }
                    } else {
                        if (tag.Name == "CONT") {
                            strings.Add(tag.StringValue);
                        }
                    }
                }
            }

            return strings;
        }

        public static void SetTagStrings(GEDCOMTag tag, StringList strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            for (int i = tag.Count - 1; i >= 0; i--) {
                string subtag = tag[i].Name;
                if (subtag == "CONT" || subtag == "CONC") {
                    tag.Delete(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GEDCOMRecord);

                int num = strings.Count;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag("CONT", sub, null);
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                        tag.AddTag("CONC", str.Substring(0, len), null);
                        str = str.Remove(0, len);
                    }
                }
            }
        }

        public static void SetTagStrings(GEDCOMTag tag, string[] strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            for (int i = tag.Count - 1; i >= 0; i--) {
                string subtag = tag[i].Name;
                if (subtag == "CONT" || subtag == "CONC") {
                    tag.Delete(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GEDCOMRecord);

                int num = strings.Length;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag("CONT", sub, null);
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                        tag.AddTag("CONC", str.Substring(0, len), null);
                        str = str.Remove(0, len);
                    }
                }
            }
        }


        public bool GetTagYNValue(string tagName)
        {
            GEDCOMTag tag = FindTag(tagName, 0);
            return (tag != null) && (tag.StringValue == "Y");
        }

        public void SetTagYNValue(string tagName, bool value)
        {
            if (value) {
                GEDCOMTag tag = FindTag(tagName, 0);
                if (tag == null) {
                    tag = AddTag(tagName, "", null);
                }
                tag.StringValue = "Y";
            } else {
                DeleteTag(tagName);
            }
        }

        #endregion

        #region Tree management

        public virtual void Pack()
        {
            fTags.Pack();
        }

        public virtual void ReplaceXRefs(XRefReplacer map)
        {
            fTags.ReplaceXRefs(map);
        }

        public void ResetOwner(GEDCOMObject owner)
        {
            fOwner = owner;
        }

        #endregion

        #region Stream management

        internal virtual GEDCOMParseFunc GetParseFunc()
        {
            return GEDCOMParseFunc.Default;
        }

        protected void SaveTagsToStream(StreamWriter stream)
        {
            int subtagsCount = fTags.Count;
            if (subtagsCount > 0) {
                for (int i = 0; i < subtagsCount; i++) {
                    GEDCOMTag subtag = fTags[i];
                    if (subtag.Name == "CONC" || subtag.Name == "CONT") {
                        subtag.SaveToStream(stream);
                    }
                }

                for (int i = 0; i < subtagsCount; i++) {
                    GEDCOMTag subtag = fTags[i];
                    if (subtag.Name != "CONT" && subtag.Name != "CONC") {
                        subtag.SaveToStream(stream);
                    }
                }
            }
        }

        protected static void WriteTagLine(StreamWriter stream, int level, string tagName, string tagValue, bool skipEmpty = false)
        {
            bool isEmpty = string.IsNullOrEmpty(tagValue);
            if (isEmpty && skipEmpty) return;

            string str = level.ToString() + " " + tagName;
            if (!string.IsNullOrEmpty(tagValue)) {
                str = str + " " + tagValue;
            }
            stream.Write(str + GEDCOMProvider.GEDCOM_NEWLINE);
        }

        protected virtual void SaveValueToStream(StreamWriter stream)
        {
            WriteTagLine(stream, fLevel, fName, StringValue);
        }

        public virtual void SaveToStream(StreamWriter stream)
        {
            SaveValueToStream(stream);
            SaveTagsToStream(stream);
        }

        #endregion
    }
}
