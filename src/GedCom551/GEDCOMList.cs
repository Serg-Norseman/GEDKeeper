using System;
using System.Collections.Generic;
using System.IO;

namespace GedCom551
{
	public sealed class GEDCOMList<T> : IDisposable
	{
		private List<T> FList = null; // lazy implementation
		//private TGEDCOMObject _owner;
		private bool _disposed;

		public int Count
		{
			get {
				return ((this.FList == null) ? 0 : this.FList.Count);
			}
		}

		public T this[int Index]
		{
			get {
				return ((this.FList == null) ? default(T) : this.FList[Index]);
			}
		}

		public GEDCOMList(TGEDCOMObject AOwner)
		{
			//this._owner = AOwner;
		}

		public void Dispose()
		{
			if (!this._disposed)
			{
				this.Clear();
				//this.FList.Free(); isnot IDisposable
				this._disposed = true;
			}
		}

		public T Add(T item)
		{
			if (item != null)
			{
				if (this.FList == null)
				{
					this.FList = new List<T>();
				}

				this.FList.Add(item);
			}

			return item;
		}

		public void Clear()
		{
			if (this.FList != null)
			{
				for (int I = this.FList.Count - 1; I >= 0; I--)
				{
					(this.FList[I] as TGEDCOMObject).Free();
				}
				this.FList.Clear();
			}
		}

		public void Delete(int index)
		{
			if (this.FList != null)
			{
				(this.FList[index] as TGEDCOMObject).Free();
				this.FList.RemoveAt(index);
			}
		}

		public void DeleteObject(T item)
		{
			if (this.FList != null)
			{
				int idx = this.FList.IndexOf(item);
				if (idx >= 0)
				{
					this.Delete(idx);
				}
			}
		}

		public void Exchange(int Index1, int Index2)
		{
			if (this.FList != null)
			{
				if (Index1 >= 0 && Index1 < this.FList.Count && Index2 >= 0 && Index2 < this.FList.Count)
				{
					T tmp = this.FList[Index1];
					this.FList[Index1] = this.FList[Index2];
					this.FList[Index2] = tmp;
				}
			}
		}

		public T Extract(int index)
		{
			if (this.FList != null)
			{
				T result = this.FList[index];
				this.FList.RemoveAt(index);
				return result;
			}
			else
			{
				return default(T);
			}
		}

		public int IndexOfObject(T item)
		{
			return (this.FList == null) ? -1 : this.FList.IndexOf(item);
		}

		public void SaveToStream(StreamWriter AStream)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int I = 0; I <= num; I++)
				{
                    if (this.FList[I] is TGEDCOMTag)
					{
                        (this.FList[I] as TGEDCOMTag).SaveToStream(AStream);
					}
				}
			}
		}

		public void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int i = 0; i <= num; i++)
				{
                    if (this.FList[i] is TGEDCOMTag)
					{
                        (this.FList[i] as TGEDCOMTag).ReplaceXRefs(aMap);
					}
				}
			}
		}

		public void ResetOwner(TGEDCOMTree AOwner)
		{
			if (this.FList != null)
			{
				int num = this.FList.Count - 1;
				for (int i = 0; i <= num; i++)
				{
                    (this.FList[i] as TGEDCOMTag).ResetOwner(AOwner);
				}
			}
			//this._owner = AOwner;
		}

		public void Pack()
		{
			if (this.FList != null)
			{
				for (int i = this.FList.Count - 1; i >= 0; i--)
				{
                    if (this.FList[i] is TGEDCOMTag)
					{
                        TGEDCOMTag tag = this.FList[i] as TGEDCOMTag;
						tag.Pack();
						if (tag.IsEmpty() && tag.IsEmptySkip())
						{
							this.Delete(i);
						}
					}
				}
			}
		}
	}
}
