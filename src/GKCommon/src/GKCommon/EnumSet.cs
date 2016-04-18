using System;
using System.Runtime.InteropServices;

namespace GKCommon
{
	[StructLayout(LayoutKind.Sequential, Pack = 1)]
	public struct EnumSet<T> : ICloneable where T : IComparable, IFormattable, IConvertible
	{
		private byte[] data;

		public static EnumSet<T> Create(params T[] args)
		{
			EnumSet<T> result = new EnumSet<T>();
			result.data = new byte[32];
			result.Include(args);
			return result;
		}

		public void Include(params T[] e)
		{
			if (e == null) return;

			for (int i = 0; i < e.Length; i++) {
				this.Include(e[i]);
			}
		}

		public void Include(T elem)
		{
			byte idx = ((IConvertible)elem).ToByte(null);
			this.data[(idx >> 3)] = (byte)(this.data[(idx >> 3)] | (1 << (int)(idx & 7u)));
		}

		public void Exclude(T elem)
		{
			byte idx = ((IConvertible)elem).ToByte(null);
			this.data[(idx >> 3)] = (byte)(this.data[(idx >> 3)] & (~(1 << (int)(idx & 7u))));
		}

		public bool Contains(T elem)
		{
			byte idx = ((IConvertible)elem).ToByte(null);
			return ((uint)this.data[(idx >> 3)] & (1 << (int)(idx & 7u))) > 0u;
		}

		public bool ContainsAll(params T[] e)
		{
            if (e == null || e.Length == 0) return false;

			for (int i = 0; i < e.Length; i++) {
				if (!this.Contains(e[i])) {
					return false;
				}
			}
			return true;
		}

		public bool HasIntersect(params T[] e)
		{
            if (e == null || e.Length == 0) return false;

			for (int i = 0; i < e.Length; i++) {
				if (this.Contains(e[i])) {
					return true;
				}
			}
			return false;
		}

		public void Clear()
		{
			for (int i = 0; i <= 31; i++) {
				this.data[i] = 0;
			}
		}

		public bool IsEmpty()
		{
			for (int i = 0; i <= 31; i++) {
				if (this.data[i] != 0) {
					return false;
				}
			}
			return true;
		}

		public static bool operator ==(EnumSet<T> left, EnumSet<T> right)
		{
			for (int I = 0; I <= 31; I++) {
				if (left.data[I] != right.data[I]) {
					return false;
				}
			}
			return true;
		}

		public static bool operator !=(EnumSet<T> left, EnumSet<T> right)
		{
			return !(left == right);
		}

		public static EnumSet<T> operator +(EnumSet<T> left, EnumSet<T> right)
		{
			EnumSet<T> result = left;
			for (int I = 0; I <= 31; I++) {
				result.data[I] |= right.data[I];
			}
			return result;
		}

		public static EnumSet<T> operator -(EnumSet<T> left, EnumSet<T> right)
		{
			EnumSet<T> result = left;
			for (int I = 0; I <= 31; I++) {
				result.data[I] = (byte)(result.data[I] & (~right.data[I]));
			}
			return result;
		}

		public static EnumSet<T> operator *(EnumSet<T> left, EnumSet<T> right)
		{
			EnumSet<T> result = left;
			for (int I = 0; I <= 31; I++) {
				result.data[I] &= right.data[I];
			}
			return result;
		}

		public string ByteToStr(int index)
		{
			return this.ByteToStr(this.data[index]);
		}

		public string ByteToStr(byte val)
		{
			uint bt = 1;
			string res = "";

			for (int i = 1; i <= 8; i++) {
				if ((val & bt) > 0) {
					res = "1" + res;
				} else {
					res = "0" + res;
				}

				bt = bt << 1;
			}

			return res;
		}

		public override string ToString()
		{
			string res = "";
			for (int i = 0; i <= 31; i++) {
				string bt = this.ByteToStr(this.data[i]);
				res = bt + res;
			}
			return res;
		}

		public override int GetHashCode()
		{
			return this.data.GetHashCode();
		}

		public override bool Equals(object obj)
		{
			if (!(obj is EnumSet<T>)) return false;

			EnumSet<T> setObj = (EnumSet<T>)obj;
			return (this == setObj);
		}

		// ICloneable
		public object Clone()
		{
			EnumSet<T> result = new EnumSet<T>();
			result.data = new byte[32];
			Array.Copy(this.data, result.data, 32);
			return result;
		}
	}
}
