using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.UI;

public class Simulation
{
    public Simulation(String dataFile, String name) {
        this.dataFile = dataFile;
        this.name = name;
    }

    public string dataFile;
    public string name;
    public CoordsTime coordsTime;
    public GameObject[] balls;
    public float[] prevBallHeights; // used to visualize a bounce.

    public void initialize(CoordsTime coordsTime) {
        this.coordsTime = coordsTime;
    }
}

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    public GameObject hitSpot;

    private GUIStyle guiStyle = new GUIStyle();

    // private string dataFileName = "Game";

    // private CoordsTime coordsTime;
    // private GameObject[] balls;
    private Simulation[] simulations;

    // SIMULATION
    private int current = 0;
    private int current_simulation = 0;

    // UI-SETTINGS
    private int speed = 2;
    private Boolean game_enabled = false;

    // Start is called before the first frame update
    void Start()
    {
        simulations = new Simulation[2];
        simulations[0] = LoadData("Training_serve_data", "Training");
        simulations[1] = LoadData("Game", "Game");
    }

    private Simulation LoadData(String dataFile, String name)
    {
        Simulation simulation = new Simulation(dataFile, name);
        TextAsset data = Resources.Load (dataFile) as TextAsset;
        CoordsTime coordsTime = JsonUtility.FromJson<CoordsTime>(data.text);
        simulation.initialize(coordsTime);

        // instantiate balls
        int ballCount = simulation.coordsTime.coords_time[0].balls.Length;
        simulation.balls = new GameObject[ballCount];
        simulation.prevBallHeights = new float[ballCount];
        for (int i = 0; i < ballCount; i++)
        {
            simulation.balls[i] = Instantiate(
                tennisBall, new Vector3(0, 0, 0), Quaternion.identity
            );

            // instantiate minBallHeights
            simulation.prevBallHeights[i] = float.MaxValue;
        }


        print(simulation.balls.Length);
        return simulation;
    }


    void OnGUI()
    {
        Simulation sim = simulations[current_simulation];
        if (current >= sim.coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        int px = 30;
        guiStyle.fontSize = px;

        GUI.Label(new Rect(0, px * 0, 100, px), 
            "FPS: " + (int)(1.0f / Time.smoothDeltaTime), guiStyle);

        GUI.Label(new Rect(0, px * 1, 100, px), 
            "Generation: " + 
            sim.coordsTime.coords_time[current].generation, guiStyle);
    }

    // Update is called once per frame
    void Update()
    {
        if (!game_enabled) {
            return;
        }

        Simulation sim = simulations[current_simulation];
        if (current >= sim.coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        CoordTime coordTime = sim.coordsTime.coords_time[current];
        for (int i = 0; i < coordTime.balls.Length; i++)
        {
            ObjectCoords ball = coordTime.balls[i];
            float z = (float) ball.x;
            float x = (float) ball.y;
            float y = (float) ball.z;
            sim.balls[i].transform.position = new Vector3(x, y, z);

            // falling down
            if (y < sim.prevBallHeights[i]) {
                sim.prevBallHeights[i] = y;
            } else /*y > prev*/ // going up. threshold.
                if (y < 0.3f && 
                sim.prevBallHeights[i] != float.MinValue) {
                // hitspot.
                Instantiate(
                    hitSpot, new Vector3(x, 0.03f, z), Quaternion.identity
                );
                sim.prevBallHeights[i] = float.MinValue;
            }
            if (y >= 0.3f && sim.prevBallHeights[i] == float.MinValue) {
                sim.prevBallHeights[i] = y;
            }
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
        btn.GetComponentInChildren<Text>().text = game_enabled ? "Stop" : "Start";
    }

    public void DropdownValueChanged(Dropdown change)
    {
        setCurrentBallsActive(false);
        current_simulation = change.value;
        setCurrentBallsActive(true);
    }

    private void setCurrentBallsActive(Boolean active) {
        foreach (var ball in simulations[current_simulation].balls)
        {
            ball.SetActive(active);
        }
    }
}
