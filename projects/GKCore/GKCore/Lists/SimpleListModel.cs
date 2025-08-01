/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;

namespace GKCore.Lists
{
    public abstract class SimpleListModel<T> : ListSource<T>
        where T : class
    {
        private IList<T> fDataSource;


        public IList<T> DataSource
        {
            get { return fDataSource; }
            set { fDataSource = value; }
        }


        protected SimpleListModel() : base()
        {
        }

        protected SimpleListModel(IBaseContext baseContext, ListColumns defaultListColumns) :
            base(baseContext, defaultListColumns)
        {
        }

        public override void UpdateContents()
        {
            if (fDataSource == null) return;

            try {
                int contentSize = fDataSource.Count;
                InitContent(contentSize);
                for (int i = 0; i < contentSize; i++) {
                    AddFilteredContent(fDataSource[i]);
                }
                DoneContent();
            } catch (Exception ex) {
                Logger.WriteError("SimpleListModel.UpdateContents()", ex);
            }
        }
    }
}
