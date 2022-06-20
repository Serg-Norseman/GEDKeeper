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
using BSLib;
using BSLib.Design.MVP;
using GDModel;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

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
        lbMoveDown
    }


    public interface ISheetList : IBaseControl
    {
        EnumSet<SheetButton> Buttons { get; set; }
        ISheetModel ListModel { get; set; }
        IListViewEx ListView { get; }
        bool ReadOnly { get; set; }

        void UpdateSheet();
    }


    public interface ISheetModel
    {
        EnumSet<RecordAction> AllowedActions { get; set; }
        GDMObject DataOwner { get; set; }
        ISheetList SheetList { get; set; }
        bool ColumnsHaveBeenChanged { get; set; }

        void Modify(object sender, ModifyEventArgs eArgs);
        void UpdateColumns(IListViewEx listView);
        void UpdateContents();
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class SheetModel<T> : ListSource<T>, ISheetModel
    {
        protected ISheetList fSheetList;
        protected readonly IBaseWindow fBaseWin;
        protected readonly ChangeTracker fUndoman;
        protected GDMObject fDataOwner;


        public GDMObject DataOwner
        {
            get {
                return fDataOwner;
            }
            set {
                fDataOwner = value;
                UpdateContents();
            }
        }

        public ISheetList SheetList
        {
            get {
                return fSheetList;
            }
            set {
                if (fSheetList != value) {
                    fSheetList = value;
                }
            }
        }

        protected SheetModel(IBaseWindow baseWin, ChangeTracker undoman) :
            base(baseWin.Context, new ListColumns<T>())
        {
            fBaseWin = baseWin;
            fUndoman = undoman;
        }

        public abstract void Modify(object sender, ModifyEventArgs eArgs);
    }
}
