/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    ///
    /// </summary>
    public abstract class ListSource<T> : IListSource<T>
    {
        protected sealed class MapColumnRec
        {
            public byte ColType;
            public byte ColSubtype;

            public MapColumnRec(byte colType, byte colSubtype)
            {
                ColType = colType;
                ColSubtype = colSubtype;
            }
        }


        private EnumSet<RecordAction> fAllowedActions;
        private bool fColumnsHaveBeenChanged;

        protected readonly IBaseContext fBaseContext;
        protected readonly List<MapColumnRec> fColumnsMap;
        protected readonly ListColumns<T> fListColumns;


        public EnumSet<RecordAction> AllowedActions
        {
            get { return fAllowedActions; }
            set { fAllowedActions = value; }
        }

        public IBaseContext BaseContext
        {
            get { return fBaseContext; }
        }

        public bool ColumnsHaveBeenChanged
        {
            get { return fColumnsHaveBeenChanged; }
            set { fColumnsHaveBeenChanged = value; }
        }

        public IListColumns ListColumns
        {
            get { return fListColumns; }
        }


        protected ListSource(IBaseContext baseContext, ListColumns<T> defaultListColumns)
        {
            fAllowedActions = new EnumSet<RecordAction>();
            fBaseContext = baseContext;
            fColumnsMap = new List<MapColumnRec>();
            fListColumns = defaultListColumns;
        }

        protected void AddColumn(IListViewEx list, string caption, int width, bool autoSize, byte colType, byte colSubtype)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            list.AddColumn(caption, width, autoSize);
            fColumnsMap.Add(new MapColumnRec(colType, colSubtype));
        }

        protected void ColumnsMap_Clear()
        {
            fColumnsMap.Clear();
        }

        public virtual void UpdateColumns(IListViewEx listView)
        {
            if (listView == null) return;

            ColumnsMap_Clear();

            int num = fListColumns.Count;
            for (int i = 0; i < num; i++) {
                ListColumn cs = fListColumns.OrderedColumns[i];

                AddColumn(listView, LangMan.LS(cs.ColName), cs.CurWidth, false, cs.Id, 0);
            }

            ColumnsHaveBeenChanged = false;
        }

        public abstract void UpdateContents();
    }
}
