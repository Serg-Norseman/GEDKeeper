using System;
using System.Runtime.InteropServices;
using System.Security;

namespace GKTreeVizPlugin
{
    public sealed class HighResolutionTimer
	{
		public enum TimerType { None, QueryPerformanceCounter, TimeGetTime, GetTimeOfDay }

		private static TimerType fType = TimerType.None;
		private static ulong fFrequency = 0;

		private bool fIsRunning = false;
		private ulong fStartCount = 0;
		private ulong fEndCount = 0;

		public ulong Count
		{
			get { return GetCurrentCount(); }
		}

		public ulong Difference
		{
			get { return (fEndCount - fStartCount); }
		}

		public double Elapsed
		{
			get { return ((fEndCount - fStartCount) / (double) fFrequency); }
		}

		public ulong EndCount
		{
			get { return fEndCount; }
		}

		public ulong Frequency
		{
			get { return fFrequency; }
		}

		public bool IsRunning
		{
			get { return fIsRunning; }
		}

		public double Resolution
		{
			get { return (1.0d / fFrequency); }
		}

		public ulong StartCount
		{
			get { return fStartCount; }
		}

		public TimerType Type
		{
			get { return fType; }
		}

		static HighResolutionTimer()
		{
			bool test;
			ulong testTime = 0;

			// Try The Windows QueryPerformanceCounter.
			try {
                test = QueryPerformanceFrequency(ref fFrequency);
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
                    testTime = timeGetTime();
				}
				catch(DllNotFoundException e) {
					Console.WriteLine(e.ToString());
					test = false;
				}
				catch(EntryPointNotFoundException e) {
					Console.WriteLine(e.ToString());
					test = false;
				}

				if (test && testTime != 0) {
					fType = TimerType.TimeGetTime;
					fFrequency = 1000;
				}
			}
		}

		private static ulong GetCurrentCount()
		{
			ulong tmp = 0;

			if (fType == TimerType.QueryPerformanceCounter) {
                QueryPerformanceCounter(ref tmp);
				return tmp;
			}
		    if (fType == TimerType.TimeGetTime) {
		        tmp = timeGetTime();
		        return tmp;
		    }
		    return 0;
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

		#region NativeMethods

        [DllImport("kernel32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool QueryPerformanceFrequency(ref ulong frequencyCount);

        [DllImport("kernel32.dll", SetLastError = true), SuppressUnmanagedCodeSecurity]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool QueryPerformanceCounter(ref ulong performanceCount);

        [DllImport("winmm.dll")]
        private static extern /*ulong*/uint timeGetTime();

        #endregion
	}
}