using GedCom551;
using GKSys;
using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKCore
{
	public class TUndoManager : IDisposable
	{
		public delegate void TTransactionEvent(object Sender, TUndoManager.TTransactionEventArg Arg);

		public enum TUndoManType : byte
		{
			autoCommit,
			manualCommit
		}

		public enum TTransactionEventArg : byte
		{
			taCommit,
			taCommitUndo,
			taCommitRedo,
			taRollback
		}

		internal int FDepth;
		internal TUndoManager.TTransactionEvent FOnTransaction;
		internal TStack FStackUndo;
		internal TStack FStackRedo;
		internal TGEDCOMTree FTree;
		internal TUndoManager.TUndoManType FType;
		protected internal bool Disposed_;

		[Browsable(false)]
		public event TUndoManager.TTransactionEvent OnTransaction
		{
			[MethodImpl(32)]
			add
			{
				this.set_OnTransaction(value);
			}
			[MethodImpl(32)]
			remove
			{
				if (this.get_OnTransaction() == value)
				{
					this.set_OnTransaction(null);
				}
			}
		}

		[Browsable(false)]
		public int Depth
		{
			get
			{
				return this.FDepth;
			}
			set
			{
				this.FDepth = value;
			}
		}

		[Browsable(false)]
		public TGEDCOMTree Tree
		{
			get
			{
				return this.FTree;
			}
		}

		protected void OnIdle(object Sender, ref bool Done)
		{
			this.Commit();
		}

		protected void Transaction(TUndoManager.TTransactionEventArg Arg)
		{
			if (this.FOnTransaction != null)
			{
				this.FOnTransaction(this, Arg);
			}
		}

		public TUndoManager(TGEDCOMTree aTree, TUndoManager.TUndoManType aType)
		{
			this.FDepth = 1000;
			this.FTree = aTree;
			this.FType = aType;
			this.FStackUndo = new TStack();
			this.FStackRedo = new TStack();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FStackUndo.Dispose();
				this.FStackRedo.Dispose();
				this.Disposed_ = true;
			}
		}

		public bool CmdDo(TCustomCommand cmd)
		{
			bool Result;
			if (!cmd.Redo())
			{
				this.Rollback();
				Result = false;
			}
			else
			{
				this.FStackUndo.Push(cmd);
				this.FStackRedo.Clear();
				Result = true;
			}
			return Result;
		}

		public void CmdUndo()
		{
			if (this.FStackUndo.Count() >= 2)
			{
				if (this.FStackUndo.Peek() == null)
				{
					this.FStackUndo.Pop();
				}
				this.FStackRedo.Push(null);
				while (this.FStackUndo.Peek() != null)
				{
					TCustomCommand cmd = this.FStackUndo.Pop() as TCustomCommand;
					this.FStackRedo.Push(cmd);
					cmd.Undo();
				}
				this.Transaction(TUndoManager.TTransactionEventArg.taCommitUndo);
			}
		}

		public void CmdRedo()
		{
			if (this.FStackRedo.Count() != 0)
			{
				if (this.FStackUndo.Peek() != null)
				{
					this.FStackUndo.Push(null);
				}
				while (this.FStackRedo.Peek() != null)
				{
					TCustomCommand cmd = this.FStackRedo.Pop() as TCustomCommand;
					this.FStackUndo.Push(cmd);
					if (!cmd.Redo())
					{
						this.Rollback();
						return;
					}
				}
				this.FStackRedo.Pop();
				this.FStackUndo.Push(null);
				this.Transaction(TUndoManager.TTransactionEventArg.taCommitRedo);
			}
		}

		public bool CanUndo()
		{
			return this.FStackUndo.Count() - 1 > 0;
		}

		public bool CanRedo()
		{
			return this.FStackRedo.Count() - 1 > 0;
		}

		public void Commit()
		{
			TCustomCommand cmd = this.FStackUndo.Peek() as TCustomCommand;
			if (cmd != null)
			{
				this.FStackUndo.Push(null);
				this.Transaction(TUndoManager.TTransactionEventArg.taCommit);
			}
		}

		public void Rollback()
		{
			while (this.FStackUndo.Peek() != null)
			{
				TCustomCommand cmd = this.FStackUndo.Pop() as TCustomCommand;
				cmd.Undo();
			}
			this.Transaction(TUndoManager.TTransactionEventArg.taRollback);
		}

		public void Clear()
		{
			this.FStackUndo.Clear();
			this.FStackUndo.Push(null);
			this.FStackRedo.Clear();
		}

		[MethodImpl(32)]
		public TUndoManager.TTransactionEvent get_OnTransaction()
		{
			return this.FOnTransaction;
		}

		[MethodImpl(32)]
		public void set_OnTransaction(TUndoManager.TTransactionEvent Value)
		{
			this.FOnTransaction = Value;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
