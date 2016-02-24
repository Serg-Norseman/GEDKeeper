using System;

namespace BSLib
{
    public struct Range<T> where T : IComparable<T>
	{
		public readonly T Start;
		public readonly T End;

		public Range(T start, T end)
		{
            if (start.CompareTo(end) > 0)
                throw new ArgumentException("End must be greater than Start");

			this.Start = start;
			this.End = end;
		}

        public bool IsOverlapped(Range<T> other)
        {
            if (this.Start.CompareTo(other.Start) == 0)
            {
                return true;
            }
            
            if (this.Start.CompareTo(other.Start) > 0)
            {
                return this.Start.CompareTo(other.End) <= 0;
            }
            
            return other.Start.CompareTo(this.End) <= 0;
        }
	}
}
