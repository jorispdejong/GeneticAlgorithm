using System.Collections.Generic;
using UnityEngine;

[System.Serializable]    
public class ObjectCoords : System.Object
{
    public double x;
    public double y;
    public double z;
}


[System.Serializable]
public class CoordTime : System.Object
{
    public int point;
    public int player;
    public float time;
    public int generation;
    public ObjectCoords[] balls;
}
