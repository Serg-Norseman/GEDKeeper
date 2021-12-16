/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Net;

namespace GKMap
{
    public sealed class EmptyWebProxy : IWebProxy
    {
        public static readonly EmptyWebProxy Instance = new EmptyWebProxy();

        private ICredentials fCredentials;

        public ICredentials Credentials
        {
            get {
                return fCredentials;
            }
            set {
                fCredentials = value;
            }
        }

        public Uri GetProxy(Uri destination)
        {
            return destination;
        }

        public bool IsBypassed(Uri host)
        {
            return true;
        }
    }
}
