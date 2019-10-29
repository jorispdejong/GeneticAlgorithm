using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.UI;

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    // public GameObject tennisBallInstance;
    private List<Dictionary<string,object>> data;
    private int current = 0;

    private GUIStyle guiStyle = new GUIStyle();

    // private string dataFileName = "test_data";
    private string dataFileName = "Game";

    private CoordsTime coordsTime;
    private GameObject[] balls;

    private int speed = 2;

    private Boolean game_enabled = true;

    private const int SIMULATE_TRAINING = 0;
    private const int SIMULATE_MATCH = 1;

    // Start is called before the first frame update
    void Start()
    {
        LoadData();
    }

    private void LoadData()
    {
        TextAsset data = Resources.Load (dataFileName) as TextAsset;
        coordsTime = JsonUtility.FromJson<CoordsTime>(data.text);

        // instantiate balls
        int ballCount = coordsTime.coords_time[0].balls.Length;
        balls = new GameObject[ballCount];
        for (int i = 0; i < ballCount; i++)
        {
            balls[i] = Instantiate(
                tennisBall, new Vector3(0, 0, 0), Quaternion.identity
            );
        }

        print(balls.Length);
    }


    void OnGUI()
    {
        if (current >= coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        int px = 30;
        guiStyle.fontSize = px;

        GUI.Label(new Rect(0, px * 0, 100, px), 
            "FPS: " + (int)(1.0f / Time.smoothDeltaTime), guiStyle);

        GUI.Label(new Rect(0, px * 1, 100, px), 
            "Generation: " + 
            coordsTime.coords_time[current].generation, guiStyle);
    }

    // Update is called once per frame
    void Update()
    {
        if (!enabled) {
            return;
        }

        if (current >= coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        CoordTime coordTime = coordsTime.coords_time[current];
        for (int i = 0; i < coordTime.balls.Length; i++)
        {
            ObjectCoords ball = coordTime.balls[i];
            float z = (float) ball.x;
            float x = (float) ball.y;
            float y = (float) ball.z;
            balls[i].transform.position = new Vector3(x, y, z);
        }

        int fps = (int) (1.0f / Time.smoothDeltaTime);
        if (fps < 0) {
            current += speed;
        } else {
            // current += fps;
            current += speed;
        }
    }
    public void OnSliderValueChanged(float value)
    {
        Debug.Log("New wind direction: " + value);
        speed = (int) value;
    }

    public void onClick(Button btn)
    {
        game_enabled = !game_enabled;
    }

    public void DropdownValueChanged(Dropdown change)
    {
        Debug.Log("dd"+change.value);
    }
}
