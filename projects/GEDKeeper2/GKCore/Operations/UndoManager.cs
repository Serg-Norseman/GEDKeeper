/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Collections.Generic;
using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Operations
{
    public enum TransactionType
    {
        taCommit,
        taCommitUndo,
        taCommitRedo,
        taRollback
    }

    public delegate void TransactionEventHandler(object sender, TransactionType type);

    public class UndoManager : BaseObject, IUndoManager
    {
        private const CustomOperation TRANS_DELIMITER = null;

        private TransactionEventHandler fOnTransaction;
        private readonly List<CustomOperation> fList;
        private int fCurrentIndex;


        public event TransactionEventHandler OnTransaction
        {
            add
            {
                this.fOnTransaction = value;
            }
            remove
            {
                if (this.fOnTransaction == value)
                {
                    this.fOnTransaction = null;
                }
            }
        }


        public UndoManager()
        {
            this.fList = new List<CustomOperation>();
            this.fCurrentIndex = -1;

            this.IntPush(TRANS_DELIMITER);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        #region Private methods

        private void Transaction(TransactionType type)
        {
            if (this.fOnTransaction != null)
            {
                this.fOnTransaction(this, type);
            }
        }

        private CustomOperation IntPeek()
        {
            if (this.fCurrentIndex < 0 || this.fCurrentIndex >= this.fList.Count) {
                return null;
            } else {
                return this.fList[this.fCurrentIndex];
            }
        }

        private void IntPush(CustomOperation op)
        {
            this.fList.Add(op);
            this.fCurrentIndex++;
        }

        #endregion

        public void Clear()
        {
            this.fList.Clear();
            this.fCurrentIndex = -1;

            this.IntPush(TRANS_DELIMITER);
        }

        public bool DoOperation(CustomOperation operation)
        {
            if (operation == null) return false;

            bool result;

            if (!operation.Redo())
            {
                this.Rollback();
                result = false;
            }
            else
            {
                // cut off redo-items
                int index = this.fCurrentIndex + 1;
                if (index < this.fList.Count)
                    this.fList.RemoveRange(index, this.fList.Count - index);

                // add new operation
                this.IntPush(operation);

                result = true;
            }

            return result;
        }

        public void Undo()
        {
            if (this.fCurrentIndex >= 1)
            {
                if (this.IntPeek() == TRANS_DELIMITER)
                {
                    this.fCurrentIndex--;
                }

                while (this.IntPeek() != TRANS_DELIMITER)
                {
                    CustomOperation cmd = this.fList[this.fCurrentIndex];
                    this.fCurrentIndex--;
                    cmd.Undo();
                }

                this.Transaction(TransactionType.taCommitUndo);
            }
        }

        public void Redo()
        {
            if (this.fCurrentIndex < this.fList.Count - 1)
            {
                if (this.fList[this.fCurrentIndex] == TRANS_DELIMITER)
                {
                    this.fCurrentIndex++;
                }

                while (this.IntPeek() != TRANS_DELIMITER)
                {
                    CustomOperation cmd = this.fList[fCurrentIndex];
                    this.fCurrentIndex++;

                    if (!cmd.Redo())
                    {
                        this.Rollback();
                        return;
                    }
                }

                this.Transaction(TransactionType.taCommitRedo);
            }
        }

        public bool CanUndo()
        {
            return this.fCurrentIndex > 0;
        }

        public bool CanRedo()
        {
            return this.fList.Count > 0 && this.fCurrentIndex < this.fList.Count - 1;
        }

        public void Commit()
        {
            if (this.IntPeek() != TRANS_DELIMITER)
            {
                this.IntPush(TRANS_DELIMITER);
                this.Transaction(TransactionType.taCommit);
            }
        }

        public void Rollback()
        {
            while (this.IntPeek() != TRANS_DELIMITER)
            {
                CustomOperation cmd = this.fList[this.fCurrentIndex];
                this.fCurrentIndex--;
                cmd.Undo();
            }
            this.Transaction(TransactionType.taRollback);
        }
    }
}
