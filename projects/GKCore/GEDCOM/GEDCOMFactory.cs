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
using System.Reflection;
using System.Reflection.Emit;
using GKCore;

namespace GKCommon.GEDCOM
{
    public delegate GEDCOMTag TagConstructor(GEDCOMObject owner, string tagName, string tagValue);

    public sealed class TagInfo
    {
        public readonly TagConstructor Constructor;
        public readonly AddTagHandler AddHandler;
        public readonly bool SkipEmpty;
        public readonly bool GKExtend;

        public TagInfo(TagConstructor constructor, AddTagHandler addHandler)
        {
            Constructor = constructor;
            AddHandler = addHandler;
            SkipEmpty = false;
            GKExtend = false;
        }

        public TagInfo(TagConstructor constructor, AddTagHandler addHandler, bool skipEmpty, bool extend)
        {
            Constructor = constructor;
            AddHandler = addHandler;
            SkipEmpty = skipEmpty;
            GKExtend = extend;
        }
    }

    public sealed class GEDCOMFactory
    {
        private static GEDCOMFactory fInstance;
        private readonly Dictionary<string, TagInfo> fTags;

        public static GEDCOMFactory GetInstance()
        {
            if (fInstance == null) fInstance = new GEDCOMFactory();
            return fInstance;
        }

        public GEDCOMFactory()
        {
            fTags = new Dictionary<string, TagInfo>();
        }

        public void RegisterTag(string tagName, TagConstructor constructor, AddTagHandler addHandler = null,
                                bool skipEmpty = false, bool extend = false)
        {
            TagInfo tagInfo;
            if (!fTags.TryGetValue(tagName, out tagInfo)) {
                tagInfo = new TagInfo(constructor, addHandler, skipEmpty, extend);
                fTags.Add(tagName, tagInfo);
            } else {
                //tagInfo.Constructor = constructor;
                //tagInfo.AddHandler = addHandler;
            }
        }

        public GEDCOMTag CreateTag(GEDCOMObject owner, string tagName, string tagValue)
        {
            TagInfo tagInfo;
            if (fTags.TryGetValue(tagName, out tagInfo)) {
                return tagInfo.Constructor(owner, tagName, tagValue);
            }
            return null;
        }

        public TagInfo GetTagInfo(string tagName)
        {
            TagInfo result;
            fTags.TryGetValue(tagName, out result);
            return result;
        }

        public AddTagHandler GetAddHandler(string tagName)
        {
            TagInfo tagInfo;
            if (fTags.TryGetValue(tagName, out tagInfo)) {
                return tagInfo.AddHandler;
            }
            return GEDCOMProvider.AddBaseTag;
        }

        #if !NETSTANDARD

        public static T CreateTagEx<T>(GEDCOMObject owner, string tagName, string tagValue) where T : GEDCOMTag
        {
            ConstructorInfo ctorInfo = typeof(T).GetConstructor(new[] {
                typeof(GEDCOMObject), typeof(string), typeof(string)
            });

            DynamicMethod dm = new DynamicMethod("Create", typeof(T), new Type[] {
                typeof(GEDCOMObject), typeof(string), typeof(string)
            }, typeof(T), true);

            ILGenerator il = dm.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0);
            il.Emit(OpCodes.Ldarg_1);
            il.Emit(OpCodes.Ldarg_2);
            il.Emit(OpCodes.Newobj, ctorInfo);
            il.Emit(OpCodes.Ret);

            TagConstructor ctor = (TagConstructor)dm.CreateDelegate(typeof(TagConstructor));
            return (T)ctor(owner, tagName, tagValue);
        }

        #endif
    }
}
