/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;

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


    public interface IOperation
    {
        bool Redo();
        void Undo();
    }


    public class UndoManager
    {
        private const CustomOperation TRANS_DELIMITER = null;

        private readonly List<IOperation> fList;
        private int fCurrentIndex;


        public event TransactionEventHandler OnTransaction;


        public UndoManager()
        {
            fList = new List<IOperation>();
            fCurrentIndex = -1;

            PushInternal(TRANS_DELIMITER);
        }

        #region Private methods

        private void Transaction(TransactionType type)
        {
            OnTransaction?.Invoke(this, type);
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
                    var cmd = fList[fCurrentIndex];
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
                    var cmd = fList[fCurrentIndex];
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
                var cmd = fList[fCurrentIndex];
                fCurrentIndex--;
                cmd.Undo();
            }
            Transaction(TransactionType.taRollback);
        }
    }
}
