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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Lists
{
    public delegate void ModifyEventHandler(object sender, ModifyEventArgs eArgs);
    public delegate void ItemValidatingEventHandler(object sender, ItemValidatingEventArgs e);

    /// <summary>
    /// 
    /// </summary>
    public class ModifyEventArgs : EventArgs
    {
        public RecordAction Action { get; private set; }
        public object ItemData { get; set; }
        public bool IsChanged { get; set; }

        public ModifyEventArgs(RecordAction action, object itemData)
        {
            Action = action;
            ItemData = itemData;
            IsChanged = false;
        }
    }

    public class ItemValidatingEventArgs : EventArgs
    {
        private bool fIsAvailable;
        private object fItem;

        public bool IsAvailable
        {
            get { return fIsAvailable; }
            set { fIsAvailable = value; }
        }

        public object Item
        {
            get { return fItem; }
            set { fItem = value; }
        }

        public ItemValidatingEventArgs() : this(null)
        {
        }

        public ItemValidatingEventArgs(object item)
        {
            fItem = item;
        }
    }


    public enum SheetButton
    {
        lbAdd,
        lbEdit,
        lbDelete,
        lbJump,
        lbMoveUp,
        lbMoveDown,
        lbCopy,
        lbCut,
        lbPaste
    }


    public interface ISheetList : IBaseControl, IThemedView
    {
        EnumSet<SheetButton> Buttons { get; set; }
        ISheetModel ListModel { get; set; }
        IListView ListView { get; }
        bool ReadOnly { get; set; }

        event ModifyEventHandler OnModify;

        void UpdateButtons();
        void UpdateSheet();
    }


    public interface ISheetModel : IListSource
    {
        EnumSet<RecordAction> AllowedActions { get; set; }
        IGDMObject DataOwner { get; set; }
        ISheetList SheetList { get; set; }

        Task Modify(object sender, ModifyEventArgs eArgs);

        void ShowDetails(object itemData);
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class SheetModel<T> : ListSource<T>, ISheetModel
        where T : class, IGDMObject
    {
        private EnumSet<RecordAction> fAllowedActions;
        protected ISheetList fSheetList;
        protected readonly IBaseWindow fBaseWin;
        protected readonly ChangeTracker fUndoman;
        protected IGDMObject fDataOwner;
        protected IView fOwner;
        protected IGDMList<T> fStructList;


        public EnumSet<RecordAction> AllowedActions
        {
            get { return fAllowedActions; }
            set {
                fAllowedActions = value;
                if (fSheetList != null) {
                    fSheetList.UpdateButtons();
                }
            }
        }

        public IGDMObject DataOwner
        {
            get {
                return fDataOwner;
            }
            set {
                fDataOwner = value;
                if (fSheetList != null) {
                    fSheetList.UpdateSheet();
                }
            }
        }

        public ISheetList SheetList
        {
            get { return fSheetList; }
            set { fSheetList = value; }
        }


        protected SheetModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) :
            this(owner, baseWin, undoman, new ListColumns(GKListType.ltNone))
        {
        }

        protected SheetModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman, ListColumns defaultListColumns) :
            base((baseWin != null ? baseWin.Context : null), defaultListColumns)
        {
            fAllowedActions = new EnumSet<RecordAction>();
            fBaseWin = baseWin;
            fOwner = owner;
            fUndoman = undoman;
        }

        protected void UpdateStructList(IGDMList<T> structList)
        {
            try {
                fStructList = structList;

                int contentSize = fStructList.Count;
                InitContent(contentSize);

                for (int i = 0; i < contentSize; i++) {
                    T rec = fStructList[i];
                    AddFilteredContent(rec);
                }

                DoneContent();
            } catch (Exception ex) {
                Logger.WriteError("SheetModel.UpdateStructList()", ex);
            }
        }

        public virtual async Task Modify(object sender, ModifyEventArgs eArgs)
        {
        }

        protected virtual GDMRecord GetReferenceRecord(object itemData)
        {
            return null;
        }

        public void ShowDetails(object itemData)
        {
            if (fBaseWin != null)
                BaseController.ViewRecordInfo(fOwner, fBaseWin, GetReferenceRecord(itemData));
        }
    }
}
