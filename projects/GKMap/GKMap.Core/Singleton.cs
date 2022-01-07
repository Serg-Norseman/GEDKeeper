/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Diagnostics;

namespace GKMap
{
    /// <summary>
    /// generic for singletons
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class Singleton<T> where T : new()
    {
        protected Singleton()
        {
            if (Instance != null) {
                throw (new Exception("You have tried to create a new singleton class where you should have instanced it. Replace your \"new class()\" with \"class.Instance\""));
            }
        }

        public static T Instance
        {
            get {
                if (SingletonCreator.Exception != null) {
                    throw SingletonCreator.Exception;
                }
                return SingletonCreator.Instance;
            }
        }

        private static class SingletonCreator
        {
            static SingletonCreator()
            {
                try {
                    Instance = new T();
                } catch (Exception ex) {
                    if (ex.InnerException != null) {
                        Exception = ex.InnerException;
                    } else {
                        Exception = ex;
                    }
                    Trace.WriteLine("Singleton: " + Exception);
                }
            }

            internal static readonly T Instance;
            internal static readonly Exception Exception;
        }
    }
}
