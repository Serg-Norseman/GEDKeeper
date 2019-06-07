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
using BSLib;
using GDModel.Providers.GEDCOM;
using GKCore.Types;

namespace GDModel
{
    /// <summary>
    /// The main class of the entire storage infrastructure in the GEDCOM format.
    /// The ancestor of all structural classes. Contains the main logic
    /// for reading and writing the values of tags in the terminology and
    /// according to the rules of GEDCOM.
    /// </summary>
    public class GDMTag : GDMObject
    {
        #region Protected fields

        private string fName;
        private GDMObject fOwner;
        protected string fStringValue;
        private GDMList<GDMTag> fTags;

        #endregion

        #region Public properties

        public string Name
        {
            get { return fName; }
        }

        public GDMObject Owner
        {
            get { return fOwner; }
        }

        public string StringValue
        {
            get { return GetStringValue(); }
            set { ParseString(value); }
        }

        /// <summary>
        /// Access to items of nested sub-tags.
        /// </summary>
        public GDMList<GDMTag> SubTags
        {
            get { return fTags; }
        }

        #endregion

        #region Object management

        public static GDMTag Create(GDMObject owner, string tagName, string tagValue)
        {
            return new GDMTag(owner, tagName, tagValue);
        }

        public GDMTag(GDMObject owner)
        {
            fOwner = owner;
            fTags = new GDMList<GDMTag>(this);
            fStringValue = string.Empty;
        }

        public GDMTag(GDMObject owner, string tagName, string tagValue) : this(owner)
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

        public void ResetOwner(GDMObject owner)
        {
            fOwner = owner;
        }

        public virtual void ReplaceXRefs(GDMXRefReplacer map)
        {
            fTags.ReplaceXRefs(map);
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

        public virtual GDMTree GetTree()
        {
            GDMTree owner = null;

            GDMTag current = this;
            while (current != null) {
                GDMObject parent = current.fOwner;

                var parentTag = parent as GDMTag;
                if (parentTag != null) {
                    current = parentTag;
                } else {
                    var parentTree = parent as GDMTree;
                    if (parentTree != null) {
                        owner = parentTree;
                    }
                    break;
                }
            }

            return owner;
        }

        public void SetName(string value)
        {
            if (value != null) {
                value = string.Intern(value);
            }
            fName = value;
        }

        public GDMTag AddTag(GDMTag tag)
        {
            fTags.Add(tag);
            return tag;
        }

        /// <summary>
        /// Adding nested sub-tags.
        /// </summary>
        /// <param name="tagName">A name of sub-tag.</param>
        /// <param name="tagValue">A string value of sub-tag.</param>
        /// <param name="tagConstructor">The default constructor of sub-tag.</param>
        /// <returns></returns>
        internal GDMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
        {
            GDMTag tag = null;

            if (tagConstructor != null) {
                tag = tagConstructor(this, tagName, tagValue);
            } else {
                tag = GEDCOMFactory.GetInstance().CreateTag(this, tagName, tagValue);
                if (tag == null) {
                    tag = new GDMTag(this, tagName, tagValue);
                }
            }

            return AddTag(tag);
        }

        /// <summary>
        /// Copying the sub-tags from the source to the current tag.
        /// </summary>
        /// <param name="source">A source tag.</param>
        public virtual void Assign(GDMTag source)
        {
            if (source == null) return;

            SetName(source.Name);
            ParseString(source.StringValue);

            AssignList(source.fTags, this.fTags);
        }

        protected void AssignList<T>(GDMList<T> srcList, GDMList<T> destList) where T : GDMTag
        {
            foreach (GDMTag sourceTag in srcList) {
                T copyTag = (T)Activator.CreateInstance(sourceTag.GetType(), new object[] { this, string.Empty, string.Empty });
                copyTag.Assign(sourceTag);
                destList.Add(copyTag);
            }
        }

        public virtual void Clear()
        {
            fTags.Clear();
            fStringValue = string.Empty;
        }

        public void DeleteTag(string tagName)
        {
            GDMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                fTags.DeleteAt(idx);

                tag = FindTag(tagName, idx);
            }
        }

        public GDMTag FindTag(string tagName, int startIndex)
        {
            string su = GEDCOMUtils.InvariantTextInfo.ToUpper(tagName);

            int pos = su.IndexOf('\\');
            string S = ((pos >= 0) ? su.Substring(0, pos) : su);

            GDMTag tempTag = this;

            while (true) {
                int index = ((S == su) ? startIndex : 0);

                GDMList<GDMTag> tempSubTags = tempTag.fTags;
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

        public IList<GDMTag> FindTags(string tagName)
        {
            IList<GDMTag> result = new List<GDMTag>();

            GDMTag tag = FindTag(tagName, 0);
            while (tag != null) {
                int idx = fTags.IndexOf(tag);
                result.Add(tag);

                tag = FindTag(tagName, idx + 1);
            }

            return result;
        }

        /// <summary>
        /// Get an existing or create a new subtag. Can use the creation of known and unknown tags 
        /// with the default constructor or specify the specific constructor.
        /// </summary>
        internal T GetTag<T>(string tagName, TagConstructor tagConstructor) where T : GDMTag
        {
            GDMTag result = FindTag(tagName, 0);

            if (result == null) {
                result = AddTag(tagName, "", tagConstructor);
            }

            return result as T;
        }

        public virtual bool IsEmpty()
        {
            return (string.IsNullOrEmpty(fStringValue) && (fTags.Count == 0));
        }

        public virtual float IsMatch(GDMTag tag, MatchParams matchParams)
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


        public string GetTagStringValue(string tagName)
        {
            GDMTag tag = FindTag(tagName, 0);
            string result = ((tag == null) ? "" : tag.StringValue);
            return result;
        }

        public void SetTagStringValue(string tagName, string value)
        {
            string su = tagName;

            GDMTag P = FindTag(su, 0);

            if (P != null) {
                P.StringValue = value;
            } else {
                GDMTag O = this;
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


        public static StringList GetTagStrings(GDMTag strTag)
        {
            StringList strings = new StringList();

            if (strTag != null) {
                if (strTag.StringValue != "") {
                    strings.Add(strTag.StringValue);
                }

                var subTags = strTag.SubTags;
                int num = subTags.Count;
                for (int i = 0; i < num; i++) {
                    GDMTag tag = subTags[i];

                    if (tag.Name == GEDCOMTagType.CONC) {
                        if (strings.Count > 0) {
                            strings[strings.Count - 1] = strings[strings.Count - 1] + tag.StringValue;
                        } else {
                            strings.Add(tag.StringValue);
                        }
                    } else {
                        if (tag.Name == GEDCOMTagType.CONT) {
                            strings.Add(tag.StringValue);
                        }
                    }
                }
            }

            return strings;
        }

        public static void SetTagStrings(GDMTag tag, StringList strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            var subTags = tag.SubTags;
            for (int i = subTags.Count - 1; i >= 0; i--) {
                string subtag = subTags[i].Name;
                if (subtag == GEDCOMTagType.CONT || subtag == GEDCOMTagType.CONC) {
                    subTags.DeleteAt(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GDMRecord);

                int num = strings.Count;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag(GEDCOMTagType.CONT, sub, null);
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                        tag.AddTag(GEDCOMTagType.CONC, str.Substring(0, len), null);
                        str = str.Remove(0, len);
                    }
                }
            }
        }

        public static void SetTagStrings(GDMTag tag, string[] strings)
        {
            if (tag == null) return;

            tag.StringValue = "";
            var subTags = tag.SubTags;
            for (int i = subTags.Count - 1; i >= 0; i--) {
                string subtag = subTags[i].Name;
                if (subtag == GEDCOMTagType.CONT || subtag == GEDCOMTagType.CONC) {
                    subTags.DeleteAt(i);
                }
            }

            if (strings != null) {
                bool isRecordTag = (tag is GDMRecord);

                int num = strings.Length;
                for (int i = 0; i < num; i++) {
                    string str = strings[i];

                    int len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag(GEDCOMTagType.CONT, sub, null);
                    }

                    while (str.Length > 0) {
                        len = Math.Min(str.Length, GEDCOMProvider.MAX_LINE_LENGTH);
                        tag.AddTag(GEDCOMTagType.CONC, str.Substring(0, len), null);
                        str = str.Remove(0, len);
                    }
                }
            }
        }


        public bool GetTagYNValue(string tagName)
        {
            GDMTag tag = FindTag(tagName, 0);
            return (tag != null) && (tag.StringValue == "Y");
        }

        public void SetTagYNValue(string tagName, bool value)
        {
            if (value) {
                GDMTag tag = FindTag(tagName, 0);
                if (tag == null) {
                    tag = AddTag(tagName, "", null);
                }
                tag.StringValue = "Y";
            } else {
                DeleteTag(tagName);
            }
        }

        #endregion
    }
}
