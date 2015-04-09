using System;
using ExtUtils;

namespace GKCommon
{
    public sealed class HighResolutionTimer : BaseObject
	{
		private static TimerType fType = TimerType.None;
		private static ulong fFrequency = 0;

		private bool fIsRunning = false;
		private ulong fStartCount = 0;
		private ulong fEndCount = 0;

		public ulong Count
		{
			get { 
				return GetCurrentCount();
			}
		}

		public ulong Difference
		{
			get {
				return (fEndCount - fStartCount);
			}
		}

		public double Elapsed
		{
			get {
				return (((double) fEndCount - (double) fStartCount) / (double) fFrequency);
			}
		}

		public ulong EndCount
		{
			get {
				return fEndCount;
			}
		}

		public ulong Frequency
		{
			get {
				return fFrequency;
			}
		}

		public bool IsRunning
		{
			get {
				return fIsRunning;
			}
		}

		public double Resolution
		{
			get {
				return ((double) 1.0 / (double) fFrequency);
			}
		}

		public ulong StartCount
		{
			get {
				return fStartCount;
			}
		}

		public TimerType Type
		{
			get {
				return fType;
			}
		}

		public enum TimerType { None, QueryPerformanceCounter, TimeGetTime, GetTimeOfDay }

		static HighResolutionTimer()
		{
			bool test;
			ulong testTime = 0;

			// Try The Windows QueryPerformanceCounter.
			try {
                test = Win32Native.QueryPerformanceFrequency(ref fFrequency);
			}
			catch(DllNotFoundException e) {
				Console.WriteLine(e.ToString());
				test = false;
			}
			catch(EntryPointNotFoundException e) {
				Console.WriteLine(e.ToString());
				test = false;
			}

			if (test && fFrequency != 0) {
				fType = TimerType.QueryPerformanceCounter;
			}
			else {
				try {
					test = true;
                    testTime = Win32Native.timeGetTime();
				}
				catch(DllNotFoundException e) {
					Console.WriteLine(e.ToString());
					test = false;
				}
				catch(EntryPointNotFoundException e) {
					Console.WriteLine(e.ToString());
					test = false;
				}

				if(test && testTime != 0) {
					fType = TimerType.TimeGetTime;
					fFrequency = 1000;
				}
			}
		}

		private static ulong GetCurrentCount()
		{
			ulong tmp = 0;

			if (fType == TimerType.QueryPerformanceCounter) {
                Win32Native.QueryPerformanceCounter(ref tmp);
				return tmp;
			}
			else if (fType == TimerType.TimeGetTime) {
                tmp = Win32Native.timeGetTime();
				return tmp;
			}
			else {
				return 0;
			}
		}

		public void Start()
		{
			this.fStartCount = GetCurrentCount();
			this.fIsRunning = true;
			this.fEndCount = this.fStartCount;
		}

		public void Stop()
		{
			this.fEndCount = GetCurrentCount();
			this.fIsRunning = false;
		}

		public void Reset()
		{
			Start();
		}
	}
}