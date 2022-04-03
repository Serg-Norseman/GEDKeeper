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

using System.Collections.Generic;
using BSLib;
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

    public class UndoManager : IUndoManager
    {
        private const CustomOperation TRANS_DELIMITER = null;

        private TransactionEventHandler fOnTransaction;
        private readonly List<IOperation> fList;
        private int fCurrentIndex;


        public event TransactionEventHandler OnTransaction
        {
            add {
                fOnTransaction = value;
            }
            remove {
                if (fOnTransaction == value) {
                    fOnTransaction = null;
                }
            }
        }


        public UndoManager()
        {
            fList = new List<IOperation>();
            fCurrentIndex = -1;

            PushInternal(TRANS_DELIMITER);
        }

        #region Private methods

        private void Transaction(TransactionType type)
        {
            if (fOnTransaction != null) {
                fOnTransaction(this, type);
            }
        }

        private IOperation PeekInternal()
        {
            if (fCurrentIndex < 0 || fCurrentIndex >= fList.Count)
                return null;

            return fList[fCurrentIndex];
        }

        private void PushInternal(IOperation op)
        {
            fList.Add(op);
            fCurrentIndex++;
        }

        #endregion

        public void Clear()
        {
            fList.Clear();
            fCurrentIndex = -1;

            PushInternal(TRANS_DELIMITER);
        }

        public bool DoOperation(IOperation operation)
        {
            if (operation == null) return false;

            bool result;

            if (!operation.Redo()) {
                Rollback();
                result = false;
            } else {
                // cut off redo-items
                int index = fCurrentIndex + 1;
                if (index < fList.Count)
                    fList.RemoveRange(index, fList.Count - index);

                // add new operation
                PushInternal(operation);

                result = true;
            }

            return result;
        }

        public void Undo()
        {
            if (fCurrentIndex >= 1) {
                if (PeekInternal() == TRANS_DELIMITER) {
                    fCurrentIndex--;
                }

                while (PeekInternal() != TRANS_DELIMITER) {
                    IOperation cmd = fList[fCurrentIndex];
                    fCurrentIndex--;
                    cmd.Undo();
                }

                Transaction(TransactionType.taCommitUndo);
            }
        }

        public void Redo()
        {
            if (fCurrentIndex < fList.Count - 1) {
                if (fList[fCurrentIndex] == TRANS_DELIMITER) {
                    fCurrentIndex++;
                }

                while (PeekInternal() != TRANS_DELIMITER) {
                    IOperation cmd = fList[fCurrentIndex];
                    fCurrentIndex++;

                    if (!cmd.Redo()) {
                        Rollback();
                        return;
                    }
                }

                Transaction(TransactionType.taCommitRedo);
            }
        }

        public bool CanUndo()
        {
            return (fCurrentIndex > 0);
        }

        public bool CanRedo()
        {
            return (fList.Count > 0) && (fCurrentIndex < fList.Count - 1);
        }

        public bool HasChanges()
        {
            return (fCurrentIndex > 0) && (PeekInternal() != TRANS_DELIMITER);
        }

        public void Commit()
        {
            if (PeekInternal() != TRANS_DELIMITER) {
                PushInternal(TRANS_DELIMITER);
                Transaction(TransactionType.taCommit);
            }
        }

        public void Rollback()
        {
            while (PeekInternal() != TRANS_DELIMITER) {
                IOperation cmd = fList[fCurrentIndex];
                fCurrentIndex--;
                cmd.Undo();
            }
            Transaction(TransactionType.taRollback);
        }
    }
}
