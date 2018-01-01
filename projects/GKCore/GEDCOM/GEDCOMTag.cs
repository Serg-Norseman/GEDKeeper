/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using System.IO;
using BSLib;

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
        private const int MAX_LINE_LENGTH = 248;

        #region Protected fields

        private int fLevel;
        private GEDCOMTree fOwner;
        private string fName;
        private GEDCOMObject fParent;

        protected string fStringValue;
        protected GEDCOMList<GEDCOMTag> fTags;

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

        public GEDCOMTree Owner
        {
            get { return fOwner; }
        }

        public GEDCOMObject Parent
        {
            get { return fParent; }
        }

        public string StringValue
        {
            get { return GetStringValue(); }
            set { SetStringValue(value); }
        }

        #endregion
        
        #region Object management
        
        protected virtual void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
        {
            fOwner = owner;
            fParent = parent;
            fTags = new GEDCOMList<GEDCOMTag>(this);
            fStringValue = "";

            GEDCOMTag parentTag = parent as GEDCOMTag;
            if (parentTag != null) {
                fLevel = parentTag.Level + 1;
            } else {
                fLevel = 0;
            }
        }

        public GEDCOMTag(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            CreateObj(owner, parent);

            if (tagName != "" || tagValue != "")
            {
                SetName(tagName);
                SetStringValue(tagValue);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fTags != null) {
                    fTags.Dispose();
                    fTags = null;
                }
            }
            base.Dispose(disposing);
        }

        public static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return new GEDCOMTag(owner, parent, tagName, tagValue);
        }

        #endregion
        
        #region Content management

        protected GEDCOMRecord FindRecord(string xref)
        {
            return ((fOwner == null) ? null : fOwner.XRefIndex_Find(xref));
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
            try
            {
                if (tagConstructor != null) {
                    tag = tagConstructor(fOwner, this, tagName, tagValue);
                } else {
                    tag = GEDCOMFactory.GetInstance().CreateTag(fOwner, this, tagName, tagValue);
                    if (tag == null) {
                        tag = new GEDCOMTag(fOwner, this, tagName, tagValue);
                    }
                }

                InsertTag(tag);
            }
            catch (Exception ex)
            {
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
            SetStringValue(source.StringValue);

            foreach (GEDCOMTag sourceTag in source.fTags)
            {
                GEDCOMTag copy = CreateCopy(sourceTag);
                InsertTag(copy);
            }
        }

        protected void AssignList(GEDCOMList<GEDCOMTag> srcList, GEDCOMList<GEDCOMTag> destList)
        {
            foreach (GEDCOMTag sourceTag in srcList)
            {
                GEDCOMTag copy = CreateCopy(sourceTag);
                destList.Add(copy);
            }
        }

        private GEDCOMTag CreateCopy(GEDCOMTag sourceTag)
        {
            GEDCOMTag result = (GEDCOMTag)Activator.CreateInstance(sourceTag.GetType(), new object[] { Owner, this, "", "" });
            result.Assign(sourceTag);
            return result;
        }

        public virtual void Clear()
        {
            fTags.Clear();
            fStringValue = "";
        }

        public void Delete(int index)
        {
            fTags.DeleteAt(index);
        }

        public void DeleteTag(string tagName)
        {
            GEDCOMTag tag = FindTag(tagName, 0);
            while (tag != null)
            {
                int idx = fTags.IndexOf(tag);
                fTags.DeleteAt(idx);

                tag = FindTag(tagName, idx);
            }
        }

        public GEDCOMTag FindTag(string tagName, int startIndex)
        {
            string su = tagName.ToUpperInvariant();

            int pos = su.IndexOf('\\');
            string S = ((pos >= 0) ? su.Substring(0, pos) : su);

            GEDCOMTag tempTag = this;

            while (true)
            {
                int index = ((S == su) ? startIndex : 0);

                while (index < tempTag.Count && tempTag[index].Name != S) index++;

                if (index >= tempTag.Count) break;

                GEDCOMTag resultTag = tempTag[index];
                tempTag = resultTag;

                pos = su.IndexOf('\\');
                if (pos >= 0)
                {
                    su = su.Substring(pos + 1);

                    pos = su.IndexOf('\\');
                    S = ((pos >= 0) ? su.Substring(0, pos) : su);
                }
                else
                {
                    su = "";
                }

                if (su == "") return resultTag;
            }

            return null;
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

        #endregion

        #region Values management

        protected virtual string GetStringValue()
        {
            return fStringValue;
        }

        protected virtual void SetStringValue(string strValue)
        {
            ParseString(strValue);
        }

        public virtual string ParseString(string strValue)
        {
            fStringValue = strValue;
            return string.Empty;
        }


        public int GetTagIntegerValue(string tagName, int defValue)
        {
            string str = GetTagStringValue(tagName);
            int result = ((str == "") ? defValue : SysUtils.ParseInt(str, defValue));
            return result;
        }

        public void SetTagIntegerValue(string tagName, int value)
        {
            SetTagStringValue(tagName, value.ToString());
        }


        public double GetTagFloatValue(string tagName, double defValue)
        {
            string str = GetTagStringValue(tagName);
            double result = ((str == "") ? defValue : SysUtils.ParseFloat(str, defValue));
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

            if (P != null)
            {
                P.StringValue = value;
            }
            else
            {
                GEDCOMTag O = this;
                while (su != "")
                {
                    string S;

                    int index = su.IndexOf('\\');
                    if (index >= 0)
                    {
                        S = su.Substring(0, index);
                        su = su.Substring(index + 1);
                    }
                    else
                    {
                        S = su;
                        su = "";
                    }

                    P = O.FindTag(S, 0);
                    if (P == null)
                    {
                        if (su == "")
                        {
                            P = O.AddTag(S, value, null);
                        }
                        else
                        {
                            P = O.AddTag(S, "", null);
                        }
                    }
                    else
                    {
                        if (su == "")
                        {
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

            if (strTag != null)
            {
                if (strTag.StringValue != "") {
                    strings.Add(strTag.StringValue);
                }

                int num = strTag.Count;
                for (int i = 0; i < num; i++)
                {
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
            for (int i = tag.Count - 1; i >= 0; i--)
            {
                if (tag[i].Name == "CONT" || tag[i].Name == "CONC")
                {
                    tag.Delete(i);
                }
            }

            if (strings != null)
            {
                bool isRecordTag = (tag is GEDCOMRecord);

                int num = strings.Count;
                for (int i = 0; i < num; i++)
                {
                    string str = strings[i];

                    int len = ((str.Length > MAX_LINE_LENGTH) ? MAX_LINE_LENGTH : str.Length);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag("CONT", sub, null);
                    }

                    while (str.Length > 0)
                    {
                        len = ((str.Length > MAX_LINE_LENGTH) ? MAX_LINE_LENGTH : str.Length);
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
            for (int i = tag.Count - 1; i >= 0; i--)
            {
                if (tag[i].Name == "CONT" || tag[i].Name == "CONC")
                {
                    tag.Delete(i);
                }
            }

            if (strings != null)
            {
                bool isRecordTag = (tag is GEDCOMRecord);

                int num = strings.Length;
                for (int i = 0; i < num; i++)
                {
                    string str = strings[i];

                    int len = ((str.Length > MAX_LINE_LENGTH) ? MAX_LINE_LENGTH : str.Length);
                    string sub = str.Substring(0, len);
                    str = str.Remove(0, len);

                    if (i == 0 && !isRecordTag) {
                        tag.StringValue = sub;
                    } else {
                        tag.AddTag("CONT", sub, null);
                    }

                    while (str.Length > 0)
                    {
                        len = ((str.Length > MAX_LINE_LENGTH) ? MAX_LINE_LENGTH : str.Length);
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

        public virtual void ResetOwner(GEDCOMTree newOwner)
        {
            fOwner = newOwner;
            fTags.ResetOwner(newOwner);
        }

        public void ResetParent(GEDCOMObject parent)
        {
            fParent = parent;
        }

        #endregion
        
        #region Stream management

        protected virtual void SaveTagsToStream(StreamWriter stream)
        {
            if (Count <= 0) return;

            StringList savedTags = new StringList();
            try
            {
                savedTags.DuplicateSolve = DuplicateSolve.Ignore;
                savedTags.Sorted = true;

                int num = Count;
                for (int i = 0; i < num; i++)
                {
                    savedTags.Add(this[i].Name);
                }

                if (savedTags.IndexOf("CONC") >= 0 || savedTags.IndexOf("CONT") >= 0)
                {
                    int num2 = Count;
                    for (int i = 0; i < num2; i++)
                    {
                        GEDCOMTag tmp = this[i];
                        
                        if (tmp.Name == "CONC" || tmp.Name == "CONT")
                        {
                            tmp.SaveToStream(stream);
                        }
                    }

                    if (savedTags.IndexOf("CONC") >= 0)
                    {
                        savedTags.Delete(savedTags.IndexOf("CONC"));
                    }
                    if (savedTags.IndexOf("CONT") >= 0)
                    {
                        savedTags.Delete(savedTags.IndexOf("CONT"));
                    }
                }

                int num3 = Count;
                for (int i = 0; i < num3; i++) {
                    GEDCOMTag tmp = this[i];
                    
                    if (tmp.Name != "CONT" && tmp.Name != "CONC") {
                        tmp.SaveToStream(stream);
                    }
                }
            }
            finally
            {
                savedTags.Dispose();
            }
        }

        protected virtual void SaveValueToStream(StreamWriter stream)
        {
            string str = fLevel.ToString() + " " + fName;

            string val = StringValue;
            if (!string.IsNullOrEmpty(val)) {
                str = str + " " + val;
            }

            stream.Write(str + GEDCOMProvider.GEDCOM_NEWLINE);
        }

        public virtual void SaveToStream(StreamWriter stream)
        {
            SaveValueToStream(stream);
            SaveTagsToStream(stream);
        }

        #endregion
    }
}
