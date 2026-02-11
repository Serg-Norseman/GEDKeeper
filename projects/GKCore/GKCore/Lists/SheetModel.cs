/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Operations;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Lists
{
    public delegate void ModifyEventHandler(object sender, ModifyEventArgs eArgs);
    public delegate void ItemValidatingEventHandler(object sender, ItemValidatingEventArgs e);


    /// <summary>
    /// Typical operations available on the records in the lists.
    /// </summary>
    public enum RecordAction
    {
        raAdd,
        raEdit,
        raDelete,
        raJump,
        raMoveUp,
        raMoveDown,
        raCopy,
        raCut,
        raPaste,
        raDetails,
    }


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
        lbPaste,
        lbDetails,
    }


    public interface ISheetList : IBaseControl, IThemedView
    {
        EnumSet<SheetButton> Buttons { get; set; }
        ISheetModel ListModel { get; set; }
        IListView ListView { get; }
        bool ReadOnly { get; set; }

        event ModifyEventHandler OnModify;

        event ItemValidatingEventHandler OnItemValidating;

        event ModifyEventHandler OnBeforeChange;

        void UpdateButtons();
        void UpdateSheet();
    }


    public interface ISheetModel : IListSource
    {
        EnumSet<RecordAction> AllowedActions { get; set; }
        List<CustomAction> CustomActions { get; }
        IGDMObject DataOwner { get; set; }
        ISheetList SheetList { get; set; }

        Task Modify(object sender, ModifyEventArgs eArgs);

        void ShowDetails(object itemData);
    }


    public sealed class CustomAction
    {
        public string Name { get; set; }
        public EventHandler<EventArgs> Handler { get; set; }

        public CustomAction(string name, EventHandler<EventArgs> handler)
        {
            Name = name;
            Handler = handler;
        }
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
        private readonly List<CustomAction> fCustomActions;


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

        public List<CustomAction> CustomActions
        {
            get { return fCustomActions; }
        }

        public IGDMObject DataOwner
        {
            get {
                return fDataOwner;
            }
            set {
                fDataOwner = value;
                OnDataOwner();
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
            fCustomActions = new List<CustomAction>();
        }

        protected virtual void OnDataOwner()
        {
            if (fSheetList != null) {
                fSheetList.UpdateSheet();
            }
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
            if (fBaseWin != null) {
                var refRec = GetReferenceRecord(itemData);
                if (refRec is GDMMultimediaRecord mediaRec) {
                    // Always a zero file, because links from other records to a multimedia cannot contain a subitem.
                    fBaseWin.ShowMedia(mediaRec, 0, false);
                } else {
                    BaseController.ViewRecordInfo(fOwner, fBaseWin, refRec);
                }
            }
        }

        protected static bool Exchange<X>(GDMList<X> list, X value, RecordAction recordAction) where X : GDMTag
        {
            bool result = false;

            int idx = list.IndexOf(value);
            if (idx < 0 || idx >= list.Count) return false;

            switch (recordAction) {
                case RecordAction.raMoveUp:
                    list.Exchange(idx - 1, idx);
                    result = true;
                    break;

                case RecordAction.raMoveDown:
                    list.Exchange(idx, idx + 1);
                    result = true;
                    break;
            }

            return result;
        }

        protected void AddCustomAction(string name, EventHandler<EventArgs> handler)
        {
            fCustomActions.Add(new CustomAction(name, handler));
        }
    }
}
