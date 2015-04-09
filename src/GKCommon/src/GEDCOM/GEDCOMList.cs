using System;
using System.Collections.Generic;
using System.IO;

namespace GKCommon.GEDCOM
{
	public interface IGEDCOMListEnumerator
	{
		object Current
		{
			get;
		}

		GEDCOMObject Owner
		{
			get;
		}

		bool MoveNext();
		void Reset();
	}

	public sealed class GEDCOMList<T> : IDisposable where T : GEDCOMObject
	{
		#region ListEnumerator

		public sealed class GEDCOMListEnumerator : IGEDCOMListEnumerator
		{
			private readonly GEDCOMList<T> fList;
			private int fIndex;

			public GEDCOMListEnumerator(GEDCOMList<T> list)
			{
				this.fList = list;
				this.fIndex = -1;
			}

			void IGEDCOMListEnumerator.Reset()
			{
				this.fIndex = -1;
			}

			GEDCOMObject IGEDCOMListEnumerator.Owner
			{
				get {
					return this.fList.fOwner;
				}
			}

			bool IGEDCOMListEnumerator.MoveNext()
			{
				this.fIndex++;
				return (this.fIndex < this.fList.Count);
			}

			object IGEDCOMListEnumerator.Current
			{
				get {
					try
					{
						return this.fList[fIndex];
					}
					catch (IndexOutOfRangeException)
					{
						throw new InvalidOperationException();
					}
				}
			}
		}

		#endregion

		
		private List<T> fList; // lazy implementation
		private readonly GEDCOMObject fOwner;
		private bool fDisposed;

		public int Count
		{
			get {
				return ((this.fList == null) ? 0 : this.fList.Count);
			}
		}

		public T this[int index]
		{
			get {
				return ((this.fList == null) ? default(T) : this.fList[index]);
			}
		}

		public GEDCOMList(GEDCOMObject owner)
		{
		    this.fOwner = owner;
            this.fList = null;
        }

	    public void Dispose()
		{
			if (!this.fDisposed)
			{
				this.Clear();
				//this.fList.Free(); isnot IDisposable
				this.fDisposed = true;
			}
		}

		public IGEDCOMListEnumerator GetEnumerator()
		{
			return new GEDCOMListEnumerator(this);
		}

		public T Add(T item)
		{
			if (item != null)
			{
				if (this.fList == null)
				{
					this.fList = new List<T>();
				}

				this.fList.Add(item);
			}

			return item;
		}

		public void Clear()
		{
			if (this.fList != null)
			{
				for (int i = this.fList.Count - 1; i >= 0; i--)
				{
					(this.fList[i] as GEDCOMObject).Dispose();
				}
				this.fList.Clear();
			}
		}

		public void Delete(int index)
		{
		    if (this.fList == null) return;
		    
            this.fList[index].Dispose();
		    this.fList.RemoveAt(index);
		}

		public void DeleteObject(T item)
		{
			if (this.fList != null)
			{
				int idx = this.fList.IndexOf(item);
				if (idx >= 0)
				{
					this.Delete(idx);
				}
			}
		}

		public void Exchange(int index1, int index2)
		{
			if (this.fList != null)
			{
				if (index1 >= 0 && index1 < this.fList.Count && index2 >= 0 && index2 < this.fList.Count)
				{
					T tmp = this.fList[index1];
					this.fList[index1] = this.fList[index2];
					this.fList[index2] = tmp;
				}
			}
		}

		public T Extract(int index)
		{
			if (this.fList != null) {
				T result = this.fList[index];
				this.fList.RemoveAt(index);
				return result;
			} else {
				return default(T);
			}
		}

		public int IndexOfObject(T item)
		{
			return (this.fList == null) ? -1 : this.fList.IndexOf(item);
		}

		public void SaveToStream(StreamWriter stream)
		{
			if (this.fList != null)
			{
				int num = this.fList.Count;
				for (int i = 0; i < num; i++) {
					T item = this.fList[i];
                    if (item is GEDCOMTag) {
                        (item as GEDCOMTag).SaveToStream(stream);
					}
				}
			}
		}

		public void ReplaceXRefs(XRefReplacer map)
		{
			if (this.fList != null)
			{
				int num = this.fList.Count;
				for (int i = 0; i < num; i++) {
					T item = this.fList[i];
                    if (item is GEDCOMTag) {
                        (item as GEDCOMTag).ReplaceXRefs(map);
					}
				}
			}
		}

		public void ResetOwner(GEDCOMTree newOwner)
		{
			if (this.fList != null)
			{
				int num = this.fList.Count;
				for (int i = 0; i < num; i++) {
                    (this.fList[i] as GEDCOMTag).ResetOwner(newOwner);
				}
			}
			//this._owner = newOwner;
		}

		public void Pack()
		{
			if (this.fList != null)
			{
				for (int i = this.fList.Count - 1; i >= 0; i--)
				{
                    if (this.fList[i] is GEDCOMTag)
					{
                        GEDCOMTag tag = this.fList[i] as GEDCOMTag;
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
