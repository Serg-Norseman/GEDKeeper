/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;

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

        protected SimpleListModel(BaseContext baseContext, ListColumns defaultListColumns) :
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
