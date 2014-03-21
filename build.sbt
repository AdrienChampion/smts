//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
//*  This file is part of Smts.                                               *
//*                                                                           *
//*  Smts is free software: you can redistribute it and/or modify             *
//*  it under the terms of the GNU Lesser General Public License as           *
//*  published by the Free Software Foundation, either version 3 of the       *
//*  License, or (at your option) any later version.                          *
//*                                                                           *
//*  Smts is distributed in the hope that it will be useful,                  *
//*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
//*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
//*  GNU Lesser General Public License for more details.                      *
//*                                                                           *
//*  You should have received a copy of the GNU Lesser General Public         *
//*  License along with Smts.                                                 *
//*  If not, see <http://www.gnu.org/licenses/>.                              *
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

name := "smts"

version := "0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-feature")

javaOptions ++= Seq("-Xmx1024M","-Xms512M")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

exportJars := true

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.3.0"
//  "commons-lang" % "commons-lang" % "2.6"
