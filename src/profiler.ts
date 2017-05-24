export class Profiler {
  startTimes: { [key: string]: [number, number] } = {};

  start(name: string): void {
    this.startTimes[name] = process.hrtime();
  }

  end(name: string): number {
    const start = this.startTimes[name];
    if (!start)
      return 0;
    const time = process.hrtime(start);
    return time[0] + time[1] * (1 / 1000000);
  }
}
