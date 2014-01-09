using System;
using System.Runtime.InteropServices;

namespace GKSandbox
{
	public sealed class HighResolutionTimer : IDisposable
	{
		private static TimerType timerType = TimerType.None;
		private static ulong timerFrequency = 0;

		private bool timerIsRunning = false;
		private ulong timerStartCount = 0;
		private ulong timerEndCount = 0;
		private static bool isDisposed = false;

		public ulong Count
		{
			get { 
				return GetCurrentCount();
			}
		}

		public ulong Difference
		{
			get {
				return (timerEndCount - timerStartCount);
			}
		}

		public double Elapsed
		{
			get {
				return (((double) timerEndCount - (double) timerStartCount) / (double) timerFrequency);
			}
		}

		public ulong EndCount
		{
			get {
				return timerEndCount;
			}
		}

		public ulong Frequency
		{
			get {
				return timerFrequency;
			}
		}

		public bool IsRunning
		{
			get {
				return timerIsRunning;
			}
		}

		public double Resolution
		{
			get {
				return ((double) 1.0 / (double) timerFrequency);
			}
		}

		public ulong StartCount
		{
			get {
				return timerStartCount;
			}
		}

		public TimerType Type
		{
			get {
				return timerType;
			}
		}

		public enum TimerType { None, QueryPerformanceCounter, TimeGetTime, GetTimeOfDay }

		static HighResolutionTimer()
		{
			bool test = false;
			ulong testTime = 0;

			// Try The Windows QueryPerformanceCounter.
			try {
				test = QueryPerformanceFrequency(ref timerFrequency);
			}
			catch(DllNotFoundException e) {
				Console.WriteLine(e.ToString());
				test = false;
			}
			catch(EntryPointNotFoundException e) {
				Console.WriteLine(e.ToString());
				test = false;
			}

			if(test && timerFrequency != 0) {
				timerType = TimerType.QueryPerformanceCounter;
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

				if(test && testTime != 0) {
					timerType = TimerType.TimeGetTime;
					timerFrequency = 1000;
				}
			}
			// TODO: Add support for *NIX
		}

		public void Dispose()
		{
			Dispose(true);
			GC.SuppressFinalize(this);
		}

		public void Dispose(bool disposing)
		{
			if (!isDisposed) {
				// Release Any Unmanaged Resources Here, If disposing Was false, Only The Following Code Is Executed
			}
			isDisposed = true;
		}

		~HighResolutionTimer()
		{
			Dispose(false);
		}

		private ulong GetCurrentCount()
		{
			ulong tmp = 0;

			if(timerType == TimerType.QueryPerformanceCounter) {
				QueryPerformanceCounter(ref tmp);
				return tmp;
			}
			else if(timerType == TimerType.TimeGetTime) {
				tmp = timeGetTime();
				return tmp;
			}
			else {
				return 0;
			}
		}

		public void Start()
		{
			this.timerStartCount = GetCurrentCount();
			this.timerIsRunning = true;
			this.timerEndCount = this.timerStartCount;
		}

		public void Stop()
		{
			this.timerEndCount = GetCurrentCount();
			this.timerIsRunning = false;
		}

		public void Reset()
		{
			Start();
		}

		[DllImport("kernel32.dll")]
		private static extern bool QueryPerformanceFrequency(ref ulong frequencyCount);

		[DllImport("kernel32.dll")]
		private static extern bool QueryPerformanceCounter(ref ulong performanceCount);

		[DllImport("winmm.dll")]
		private static extern ulong timeGetTime();
	}
}