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

using System;
using System.Collections.Generic;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Operations;

namespace GKCore
{
    public enum TransactionType
    {
        taCommit,
        taCommitUndo,
        taCommitRedo,
        taRollback
    }

    public delegate void TransactionEventHandler(object sender, TransactionType type); // (object sender, EventArgs e);

    public sealed class UndoManager : BaseObject
    {
        private const CustomOperation TRANS_DELIMITER = null;

        private TransactionEventHandler fOnTransaction;
        private Stack<CustomOperation> fStackUndo;
        private Stack<CustomOperation> fStackRedo;
        private GEDCOMTree fTree;


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

        public GEDCOMTree Tree
        {
            get { return this.fTree; }
        }


        public UndoManager(GEDCOMTree tree)
        {
            this.fTree = tree;
            this.fStackUndo = new Stack<CustomOperation>();
            this.fStackRedo = new Stack<CustomOperation>();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fStackUndo.Dispose();
                //this.fStackRedo.Dispose();
            }
            base.Dispose(disposing);
        }

        private void Transaction(TransactionType type)
        {
            if (this.fOnTransaction != null)
            {
                this.fOnTransaction(this, type);
            }
        }

        public bool DoOperation(CustomOperation operation)
        {
            bool result;

            if (!operation.Redo())
            {
                this.Rollback();
                result = false;
            }
            else
            {
                this.fStackUndo.Push(operation);
                this.fStackRedo.Clear();
                result = true;
            }

            return result;
        }

        public void Undo()
        {
            if (this.fStackUndo.Count >= 2)
            {
                if (this.fStackUndo.Peek() == TRANS_DELIMITER)
                {
                    this.fStackUndo.Pop();
                }
                this.fStackRedo.Push(TRANS_DELIMITER);
                while (this.fStackUndo.Peek() != TRANS_DELIMITER)
                {
                    CustomOperation cmd = this.fStackUndo.Pop();
                    this.fStackRedo.Push(cmd);
                    cmd.Undo();
                }
                this.Transaction(TransactionType.taCommitUndo);
            }
        }

        public void Redo()
        {
            if (this.fStackRedo.Count != 0)
            {
                if (this.fStackUndo.Peek() != TRANS_DELIMITER)
                {
                    this.fStackUndo.Push(TRANS_DELIMITER);
                }
                while (this.fStackRedo.Peek() != TRANS_DELIMITER)
                {
                    CustomOperation cmd = this.fStackRedo.Pop();
                    this.fStackUndo.Push(cmd);
                    if (!cmd.Redo())
                    {
                        this.Rollback();
                        return;
                    }
                }
                this.fStackRedo.Pop();
                this.fStackUndo.Push(TRANS_DELIMITER);
                this.Transaction(TransactionType.taCommitRedo);
            }
        }

        public bool CanUndo()
        {
            return this.fStackUndo.Count - 1 > 0;
        }

        public bool CanRedo()
        {
            return this.fStackRedo.Count - 1 > 0;
        }

        public void Commit()
        {
            CustomOperation cmd = this.fStackUndo.Peek();
            if (cmd != TRANS_DELIMITER)
            {
                this.fStackUndo.Push(TRANS_DELIMITER);
                this.Transaction(TransactionType.taCommit);
            }
        }

        public void Rollback()
        {
            while (this.fStackUndo.Peek() != TRANS_DELIMITER)
            {
                CustomOperation cmd = this.fStackUndo.Pop();
                cmd.Undo();
            }
            this.Transaction(TransactionType.taRollback);
        }

        public void Clear()
        {
            this.fStackUndo.Clear();
            this.fStackUndo.Push(TRANS_DELIMITER);
            this.fStackRedo.Clear();
        }
    }
}
