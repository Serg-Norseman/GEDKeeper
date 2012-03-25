using System;
using System.Collections;

using Ext.Utils;
using GedCom551;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class UndoManager : IDisposable
	{
		public delegate void TTransactionEvent(object Sender, UndoManager.TTransactionEventArg Arg);

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

		private int FDepth;
		private UndoManager.TTransactionEvent FOnTransaction;
		private Stack FStackUndo;
		private Stack FStackRedo;
		private TGEDCOMTree FTree;
		private UndoManager.TUndoManType FType;
		protected bool Disposed_;


		public event UndoManager.TTransactionEvent OnTransaction
		{
			add
			{
				this.FOnTransaction = value;
			}
			remove
			{
				if (this.FOnTransaction == value)
				{
					this.FOnTransaction = null;
				}
			}
		}

		public int Depth
		{
			get { return this.FDepth; }
			set { this.FDepth = value; }
		}

		public TGEDCOMTree Tree
		{
			get { return this.FTree; }
		}

		protected void OnIdle(object Sender, ref bool Done)
		{
			this.Commit();
		}

		protected void Transaction(UndoManager.TTransactionEventArg Arg)
		{
			if (this.FOnTransaction != null)
			{
				this.FOnTransaction(this, Arg);
			}
		}

		public UndoManager(TGEDCOMTree aTree, UndoManager.TUndoManType aType)
		{
			this.FDepth = 1000;
			this.FTree = aTree;
			this.FType = aType;
			this.FStackUndo = new Stack();
			this.FStackRedo = new Stack();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				//this.FStackUndo.Dispose();
				//this.FStackRedo.Dispose();
				this.Disposed_ = true;
			}
		}

		public bool CmdDo(CustomCommand cmd)
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
			if (this.FStackUndo.Count >= 2)
			{
				if (this.FStackUndo.Peek() == null)
				{
					this.FStackUndo.Pop();
				}
				this.FStackRedo.Push(null);
				while (this.FStackUndo.Peek() != null)
				{
					CustomCommand cmd = this.FStackUndo.Pop() as CustomCommand;
					this.FStackRedo.Push(cmd);
					cmd.Undo();
				}
				this.Transaction(UndoManager.TTransactionEventArg.taCommitUndo);
			}
		}

		public void CmdRedo()
		{
			if (this.FStackRedo.Count != 0)
			{
				if (this.FStackUndo.Peek() != null)
				{
					this.FStackUndo.Push(null);
				}
				while (this.FStackRedo.Peek() != null)
				{
					CustomCommand cmd = this.FStackRedo.Pop() as CustomCommand;
					this.FStackUndo.Push(cmd);
					if (!cmd.Redo())
					{
						this.Rollback();
						return;
					}
				}
				this.FStackRedo.Pop();
				this.FStackUndo.Push(null);
				this.Transaction(UndoManager.TTransactionEventArg.taCommitRedo);
			}
		}

		public bool CanUndo()
		{
			return this.FStackUndo.Count - 1 > 0;
		}

		public bool CanRedo()
		{
			return this.FStackRedo.Count - 1 > 0;
		}

		public void Commit()
		{
			CustomCommand cmd = this.FStackUndo.Peek() as CustomCommand;
			if (cmd != null)
			{
				this.FStackUndo.Push(null);
				this.Transaction(UndoManager.TTransactionEventArg.taCommit);
			}
		}

		public void Rollback()
		{
			while (this.FStackUndo.Peek() != null)
			{
				CustomCommand cmd = this.FStackUndo.Pop() as CustomCommand;
				cmd.Undo();
			}
			this.Transaction(UndoManager.TTransactionEventArg.taRollback);
		}

		public void Clear()
		{
			this.FStackUndo.Clear();
			this.FStackUndo.Push(null);
			this.FStackRedo.Clear();
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
