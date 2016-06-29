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
using System.Collections;
using GKCommon.GEDCOM;
using GKCore.Operations;

namespace GKCore
{
    public enum UndoCommitType
    {
        autoCommit,
        manualCommit
    }

    public enum TransactionType
    {
        taCommit,
        taCommitUndo,
        taCommitRedo,
        taRollback
    }

    public class UndoManager : IDisposable
    {
        public delegate void TTransactionEvent(object sender, TransactionType arg);

        private int fDepth;
        private UndoManager.TTransactionEvent fOnTransaction;
        private Stack fStackUndo;
        private Stack fStackRedo;
        private GEDCOMTree fTree;
        private UndoCommitType fType;
        protected bool fDisposed;


        public event UndoManager.TTransactionEvent OnTransaction
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

        public int Depth
        {
            get { return this.fDepth; }
            set { this.fDepth = value; }
        }

        public GEDCOMTree Tree
        {
            get { return this.fTree; }
        }

        public UndoManager(GEDCOMTree tree, UndoCommitType commitType)
        {
            this.fDepth = 1000;
            this.fTree = tree;
            this.fType = commitType;
            this.fStackUndo = new Stack();
            this.fStackRedo = new Stack();
        }

        public void Dispose()
        {
            if (!this.fDisposed)
            {
                //this.fStackUndo.Dispose();
                //this.fStackRedo.Dispose();
                this.fDisposed = true;
            }
        }

        protected void OnIdle(object sender, ref bool done)
        {
            this.Commit();
        }

        protected void Transaction(TransactionType arg)
        {
            if (this.fOnTransaction != null)
            {
                this.fOnTransaction(this, arg);
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
                if (this.fStackUndo.Peek() == null)
                {
                    this.fStackUndo.Pop();
                }
                this.fStackRedo.Push(null);
                while (this.fStackUndo.Peek() != null)
                {
                    CustomOperation cmd = this.fStackUndo.Pop() as CustomOperation;
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
                if (this.fStackUndo.Peek() != null)
                {
                    this.fStackUndo.Push(null);
                }
                while (this.fStackRedo.Peek() != null)
                {
                    CustomOperation cmd = this.fStackRedo.Pop() as CustomOperation;
                    this.fStackUndo.Push(cmd);
                    if (!cmd.Redo())
                    {
                        this.Rollback();
                        return;
                    }
                }
                this.fStackRedo.Pop();
                this.fStackUndo.Push(null);
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
            CustomOperation cmd = this.fStackUndo.Peek() as CustomOperation;
            if (cmd != null)
            {
                this.fStackUndo.Push(null);
                this.Transaction(TransactionType.taCommit);
            }
        }

        public void Rollback()
        {
            while (this.fStackUndo.Peek() != null)
            {
                CustomOperation cmd = this.fStackUndo.Pop() as CustomOperation;
                cmd.Undo();
            }
            this.Transaction(TransactionType.taRollback);
        }

        public void Clear()
        {
            this.fStackUndo.Clear();
            this.fStackUndo.Push(null);
            this.fStackRedo.Clear();
        }
    }
}
