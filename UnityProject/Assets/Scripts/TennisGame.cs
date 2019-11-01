using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.UI;
using System.Linq; 

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
    public int prevGeneration = 0; // for tracking generation change.

    public void initialize(CoordsTime coordsTime) {
        this.coordsTime = coordsTime;
    }
}

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    public GameObject hitSpot;

    private GUIStyle guiStyleTopLeft = new GUIStyle();
    private GUIStyle guiStyleTopRight = new GUIStyle();

    private Simulation[] simulations;

    // SIMULATION
    private int current = 0;
    private int current_simulation = 0;
    private const int SIM_TRAIN = 0;
    private const int SIM_GAME = 1;

    // UI-SETTINGS
    private int speed = 2;
    private Boolean game_enabled = false;

    // Start is called before the first frame update
    void Start()
    {
        simulations = new Simulation[2];
        simulations[SIM_TRAIN] = LoadData("Training_serve_data", "Training");
        simulations[SIM_GAME] = LoadData("Game", "Game");
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
        guiStyleTopLeft.fontSize = px;
        guiStyleTopRight.fontSize = px;

        GUI.Label(new Rect(0, px * 0, 100, px), 
            "FPS: " + (int)(1.0f / Time.smoothDeltaTime), guiStyleTopLeft);

        CoordTime coordTime = sim.coordsTime.coords_time[current];
        int generation = coordTime.generation;
        if (current_simulation == SIM_TRAIN) {
            GUI.Label(new Rect(0, px * 1, 100, px), 
                "Generation: " + 
                generation, guiStyleTopLeft);
            GUI.Label(new Rect(0, px * 2, 100, px), 
                "Player: " + 
                coordTime.player +
                " / " + 
                sim.coordsTime.n_players, guiStyleTopLeft);

            // player scorebord
            GenerationScore score = sim.coordsTime.scores[generation];
            // parents
            int[] currentSubset = score.score_per_player
                .Take(coordTime.player)
                .ToArray();
            var sortedIndices = currentSubset
                    .Select((v, i) => new { Value = v, Index = i })
                    .OrderByDescending(x => x.Value)
                    .Select(x => x.Index).ToArray();
            int[] parents = { sortedIndices[0] };
            if (sortedIndices.Length > 1) {
                parents = parents.Append(sortedIndices[1]).ToArray();
            }
            for (int i = 0; i < coordTime.player; i++)
            {
                int playerScore = score.score_per_player[i];
                guiStyleTopRight.alignment = TextAnchor.UpperRight;
                guiStyleTopRight.normal.textColor = 
                    Array.IndexOf(parents, i) > -1 ? 
                    Color.red : Color.black; // Color parents red
                GUI.Label(new Rect(0, px * i, Screen.width - 20, Screen.height), 
                    "Player " + (i + 1) + " score: " + 
                    playerScore, guiStyleTopRight);
            }
        }
        if (current_simulation == SIM_GAME) {
            GUI.Label(new Rect(0, px * 1, 100, px),
                "Game point: " +
                coordTime.point, guiStyleTopLeft);
        }
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
        
        // did generation change?
        if (sim.prevGeneration != coordTime.generation && 
            sim.prevGeneration != 0) {
            game_enabled = false;
        }
        // going to advance a generation.
        // if ((current + 1) < sim.coordsTime.coords_time.Length && 
        //     sim.coordsTime.coords_time[current + 1].generation > 
        //     sim.coordsTime.coords_time[current].generation) {
        //     game_enabled = false;
        // }
        // update generation
        sim.prevGeneration = coordTime.generation;

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
