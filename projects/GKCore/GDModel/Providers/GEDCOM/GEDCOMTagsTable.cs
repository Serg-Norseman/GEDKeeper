﻿/*
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

namespace GDModel.Providers.GEDCOM
{
    public class GEDCOMTagProps
    {
        public int TagId;
        public string TagName;

        public TagConstructor Constructor;
        public AddTagHandler AddHandler;
        public SaveTagHandler SaveHandler;
        public bool SkipEmpty;

        public GEDCOMTagProps(int tagId, string tagName)
        {
            TagId = tagId;
            TagName = tagName;
        }

        public GEDCOMTagProps(int tagId, string tagName,
                              TagConstructor constructor, AddTagHandler addHandler, SaveTagHandler saveHandler,
                              bool skipEmpty)
        {
            TagId = tagId;
            TagName = tagName;

            Constructor = constructor;
            AddHandler = addHandler;
            SaveHandler = saveHandler;
            SkipEmpty = skipEmpty;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public static class GEDCOMTagsTable
    {
        private static readonly Dictionary<string, GEDCOMTagProps> fDictionary;
        private static readonly Dictionary<int, GEDCOMTagProps> fList;
        private static int fLastId = 0;

        public static GEDCOMTagProps RegisterTag(GEDCOMTagType tag, string tagName)
        {
            GEDCOMTagProps tagProps;
            if (!fDictionary.TryGetValue(tagName, out tagProps)) {
                int tagId = (int)tag;
                tagProps = new GEDCOMTagProps(tagId, tagName);

                fDictionary.Add(tagName, tagProps);
                fList[tagId] = tagProps;
            } else {
                
            }
            return tagProps;
        }

        public static void RegisterTag(GEDCOMTagType tag, string tagName,
                                       TagConstructor constructor, AddTagHandler addHandler = null,
                                       SaveTagHandler saveHandler = null,
                                       bool skipEmpty = false)
        {
            GEDCOMTagProps tagProps = RegisterTag(tag, tagName);

            tagProps.Constructor = constructor;
            tagProps.AddHandler = addHandler;
            tagProps.SaveHandler = saveHandler;
            tagProps.SkipEmpty = skipEmpty;
        }

        public static int Lookup(string tagName)
        {
            GEDCOMTagProps tagProps;
            if (fDictionary.TryGetValue(tagName, out tagProps)) {
                return tagProps.TagId;
            } else {
                fLastId += 1;
                int tagId = (0x1 << 8) | fLastId;

                tagProps = new GEDCOMTagProps(tagId, tagName);

                fDictionary.Add(tagName, tagProps);
                fList[tagId] = tagProps;

                return tagId;
            }
        }

        public static GEDCOMTagProps GetTagProps(string tagName)
        {
            GEDCOMTagProps result;
            fDictionary.TryGetValue(tagName, out result);
            return result;
        }

        public static GEDCOMTagProps GetTagProps(int tagId)
        {
            GEDCOMTagProps result;
            fList.TryGetValue(tagId, out result);
            return result;
        }

        static GEDCOMTagsTable()
        {
            fDictionary = new Dictionary<string, GEDCOMTagProps>();
            fList = new Dictionary<int, GEDCOMTagProps>();
        }
    }
}
